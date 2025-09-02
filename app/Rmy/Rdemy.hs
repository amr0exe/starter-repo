{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rmy.Rdemy where

import Cardano.Api
import Cardano.Kuber.Api
import Cardano.Kuber.Util

import Data.Text (pack)
import PlutusLedgerApi.Common

import MultiSig (MsD)
import qualified Data.Map as Map
import qualified Data.Set as Set
import PlutusLedgerApi.V3 (PubKeyHash (PubKeyHash))

-- address parsing
parseAddressString :: String -> Either FrameworkError (AddressInEra ConwayEra)
parseAddressString addr =
    case deserialiseAddress (AsAddressInEra AsConwayEra) (pack addr) of
        Just addr   -> Right addr
        Nothing     -> Left $ FrameworkError ParserError "Failed to parse address "

-- signingKey
keyFromFilePath :: IO (SigningKey PaymentKey)
keyFromFilePath = readSignKey "payment.skey"

keyFromfPath :: FilePath -> IO (SigningKey PaymentKey)
keyFromfPath = readSignKey

signingKeyTopkh :: SigningKey PaymentKey -> PubKeyHash
signingKeyTopkh sk =
    let vkey = getVerificationKey sk
        bs = serialiseToRawBytes (verificationKeyHash vkey)
    in PubKeyHash (toBuiltin bs)

readPlutusFile :: FilePath -> IO (PlutusScript PlutusScriptV3)
readPlutusFile path = do
    scriptResult <- readFileTextEnvelope (AsPlutusScript AsPlutusScriptV3) (File path)
    case scriptResult of
        Left err -> do
            print err
            error "Failed to read Plutus Script file."
        Right script -> return script


utxoHasDatumHash :: Hash ScriptData -> TxOut CtxUTxO era  -> Bool
utxoHasDatumHash targetHash (TxOut _ _ datum _) =
    case datum of
        TxOutDatumNone -> False
        TxOutDatumHash _ h -> h == targetHash
        TxOutDatumInline _ sd -> hashScriptDataBytes sd == targetHash


-- query utxos
-- for building transaction
-- reference inputs in transaction
-- to consume, reference them in inputs

redeemFromMultiSig
    :: PlutusScript PlutusScriptV3
    -> AddressInEra ConwayEra
    -> MsD
    -> [SigningKey PaymentKey]
    -> SigningKey PaymentKey
    -> AddressInEra ConwayEra
    -> Kontract ChainConnectInfo w FrameworkError TxBuilder
redeemFromMultiSig multiSigScript scriptAddress multiSigParams requiredSigners collectorWalletKey collectorWalletAdress = do
    (UTxO contractUTxos) <- kQueryUtxoByAddress (Set.singleton (addressInEraToAddressAny scriptAddress))

    -- datum check
    let targetDatum = unsafeHashableScriptData $ fromPlutusData $ toData multiSigParams
    let targetDatumHash = hashScriptDataBytes targetDatum

    let matchingDatum = Map.filter (utxoHasDatumHash targetDatumHash) contractUTxos

    if Map.null matchingDatum
        then kError LibraryError "No UTxOs found with specified datum."
        else do
            let redeemer = unsafeHashableScriptData $ fromPlutusData $ toData ()

            let txBuilder =
                    mconcat (map (\(txin, txout) -> txRedeemUtxo txin txout multiSigScript redeemer Nothing) (Map.toList matchingDatum))
                    <> mconcat (map txSign requiredSigners)
                    <> txWalletSignKey collectorWalletKey
                    <> txChangeAddress collectorWalletAdress

            pure txBuilder