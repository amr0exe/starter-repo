{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS

import MultiSig (validatorSerialized, MsD(..))
import Rmy.LockAtSc (lockAtScript)
import Rmy.Rdemy (keyFromFilePath, keyFromfPath, signingKeyTopkh, parseAddressString, readPlutusFile, redeemFromMultiSig)

import Cardano.Api
    ( SerialiseAddress(serialiseAddress),
      PlutusScriptV3,
      PlutusScript,
      Script(PlutusScript),
      serialiseToRawBytesHexText,
      PlutusScriptVersion(PlutusScriptV3),
      PaymentCredential(PaymentCredentialByScript, PaymentCredentialByKey),
      StakeAddressReference(NoStakeAddress),
      hashScript,
      makeShelleyAddress,
      toAddressAny,
      Value,
      lovelaceToValue,
      Lovelace, writeFileTextEnvelope, File(..), prettyPrintJSON, IsShelleyBasedEra (shelleyBasedEra), makeShelleyAddressInEra)

import Cardano.Api.Shelley (getTxBody, getTxId, getTxBody)

import Cardano.Kuber.Api (chainInfoFromEnv, evaluateKontract, HasKuberAPI (kBuildAndSubmit), ChainConnectInfo, getNetworkFromEnv)

localNodeConnection :: IO ChainConnectInfo
localNodeConnection = chainInfoFromEnv

main :: IO ()
main = do
    args <- getArgs
    conn <- chainInfoFromEnv
    (_, network) <- getNetworkFromEnv "NETWORK"

    sk1 <- keyFromfPath "payment.skey"
    sk2 <- keyFromfPath "key1.skey"
    sk3 <- keyFromfPath "key2.skey"

    let pkhs = map signingKeyTopkh [sk1, sk2, sk3]
    let minNum = 2    
    let multiSigParams = MsD { signatories = pkhs, min_num = minNum }

    let specializedScript = validatorSerialized multiSigParams
    let scriptHash = hashScript (PlutusScript PlutusScriptV3 specializedScript)
    let specializedAddress = makeShelleyAddressInEra shelleyBasedEra network (PaymentCredentialByScript scriptHash) NoStakeAddress
    
    case args of
        ["cbor-writer"] -> do
            putStrLn  "Writing cbor-file"
            let description = "Multi-Sig validator script(V3)"
            
            errOrUnit <- writeFileTextEnvelope (File "MultiSig.plutus") (Just description) specializedScript
            case errOrUnit of
                Left err -> print err
                Right () -> putStrLn "Successfully wrote the contract to MultiSig.plutus"

        ["sAddr"] -> do
            putStrLn $ "Script address: " ++ T.unpack (serialiseAddress specializedAddress)

        ["lockAda"] -> do
            let val :: Value 
                val = lovelaceToValue 2_000_000

            result <- evaluateKontract conn $ do
                txb <- lockAtScript specializedAddress val sk1
                kBuildAndSubmit txb
            case result of
                Left e -> putStrLn $ "Error locking funds: \n" ++ show e
                Right r -> putStrLn $ "Successfully locked funds: \n" ++ show (getTxId (getTxBody r))

        ["redeemAda"] -> do
            -- collector walletAddr
            collectorWalletAdress <- case parseAddressString "addr_test1qq9lealepm0l98t86dt9p6ct4g9xk4tn2rpqclzjp8fmk8zdzvrwtcjnte6qgwtutlqvzcet3jr5sfe6wphs6fw6g5zqcua95t" of
                Left e -> fail $ "Invalid address: " ++ show e
                Right  a -> pure a

            let requiredSigners = [sk1, sk2]

            result <- evaluateKontract conn $ do
                txb <- redeemFromMultiSig specializedScript specializedAddress requiredSigners sk1 collectorWalletAdress
                liftIO $ BS.writeFile "txbuilder.json" (prettyPrintJSON txb)
                kBuildAndSubmit txb

            case result of
                Left e -> putStrLn $ "Error redeemign funds: \n" ++ show e
                Right r -> putStrLn $ "Kontract redeeming utxos successfull: \n" ++ show (getTxId (getTxBody r))

