{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T

import System.Environment (getArgs)

import MultiSig (validatorSerialized, MsD(..))
import Buildy (loadConfig, buildTHEKontract)
import Rmy.LockAtSc (lockAtScript)
import Rmy.Rdemy (keyFromFilePath, keyFromfPath, signingKeyTopkh, parseAddressString, readPlutusFile, redeemFromMultiSig)

import Cardano.Api
    ( SerialiseAddress(serialiseAddress),
      PlutusScriptV3,
      PlutusScript,
      Script(PlutusScript),
      serialiseToRawBytesHexText,
      PlutusScriptVersion(PlutusScriptV3),
      PaymentCredential(PaymentCredentialByScript),
      StakeAddressReference(NoStakeAddress),
      hashScript,
      makeShelleyAddress,
      toAddressAny,
      Value,
      lovelaceToValue,
      Lovelace, writeFileTextEnvelope)
import Cardano.Kuber.Api (chainInfoFromEnv, evaluateKontract, HasKuberAPI (kBuildAndSubmit), ChainConnectInfo, getNetworkFromEnv)
import Cardano.Api.Shelley (PlutusScript(PlutusScriptSerialised))

localNodeConnection :: IO ChainConnectInfo
localNodeConnection = chainInfoFromEnv

main :: IO ()
main = do
    args <- getArgs
    conn <- chainInfoFromEnv

    case args of
        ["cbor-writer"] -> do
            let sc :: PlutusScript PlutusScriptV3
                sc = PlutusScriptSerialised validatorSerialized
            
            let description = "Muli-Signature validator script."
            errOrUnit <- writeFileTextEnvelope "MultiSig.plutus" (Just description) sc

            case errOrUnit of
                Left err -> print err
                Right () -> do
                    putStrLn "Successfully wrote to MultiSig.plutus in the correct JSON TextEnvelop format."
                    let cborHex = serialiseToRawBytesHexText sc
                    putStrLn $ "CBOR hex: " ++ show cborHex
            -- let bytes = SBS.fromShort validatorSerialized
            -- -- write CBOR file
            -- BS.writeFile "MultiSig.plutus" bytes
            -- putStrLn $ "CBOR hex:: " ++ show (B16.encode bytes)

        ["sAddr"] -> do
            (_, network) <- getNetworkFromEnv "NETWORK"
            let ps :: PlutusScript PlutusScriptV3
                ps = PlutusScriptSerialised validatorSerialized
            let sh = hashScript (PlutusScript PlutusScriptV3 ps)
            let addr = makeShelleyAddress network (PaymentCredentialByScript sh) NoStakeAddress
            putStrLn $ "Script address: " ++ T.unpack (serialiseAddress (toAddressAny addr))

        ["lockAda"] -> do
            cfg <- loadConfig
            conn <- chainInfoFromEnv
            -- scriptAddr 
            req_addr <- case parseAddressString "addr_test1wqufm3cwyc6ea3vhdfppkx9hjaxmk2065q04wf9jjrcgplcn5xs7n" of
                Left e -> fail $ "Invalid address: " ++ show e
                Right  a -> pure a

            -- keys for datum
            sk1 <- keyFromfPath "payment.skey"
            sk2 <- keyFromfPath "key1.skey"
            sk3 <- keyFromfPath "key2.skey"

            -- datum
            let datumToattach = MsD
                    { signatories = map signingKeyTopkh [sk1, sk2, sk3]
                    , min_num = 2
                    }
            keyR <- keyFromFilePath

            let val :: Value
                val = lovelaceToValue 2_000_000

            result <- evaluateKontract conn $ do
                txb <- lockAtScript req_addr val datumToattach keyR
                kBuildAndSubmit txb
            case result of
                Left e -> putStrLn $ "unexpected error while evaluating contract: \n" ++ show e
                Right r -> putStrLn $ "Kontract execution successfull: \n" ++ show r

        ["redeemAda"] -> do
            conn <- chainInfoFromEnv

            -- script file
            multiSigSC <- readPlutusFile "MultiSig.plutus"
            
            -- scriptAddr
            req_addr <- case parseAddressString "addr_test1wqufm3cwyc6ea3vhdfppkx9hjaxmk2065q04wf9jjrcgplcn5xs7n" of
                Left e -> fail $ "Invalid address: " ++ show e
                Right  a -> pure a
            
            -- collector walletAddr
            collectorWalletAdress <- case parseAddressString "addr_test1qq9lealepm0l98t86dt9p6ct4g9xk4tn2rpqclzjp8fmk8zdzvrwtcjnte6qgwtutlqvzcet3jr5sfe6wphs6fw6g5zqcua95t" of
                Left e -> fail $ "Invalid address: " ++ show e
                Right  a -> pure a
        
            -- keys for datum
            sk1 <- keyFromfPath "payment.skey"
            sk2 <- keyFromfPath "key1.skey"
            sk3 <- keyFromfPath "key2.skey"

            -- datum
            let datumToattach = MsD
                    { signatories = map signingKeyTopkh [sk1, sk2, sk3]
                    , min_num = 2
                    }
            let requiredSigners = [sk1, sk2, sk3]

            result <- evaluateKontract conn $ do
                txb <- redeemFromMultiSig multiSigSC req_addr datumToattach requiredSigners sk1 collectorWalletAdress
                kBuildAndSubmit txb
            
            case result of
                Left e -> putStrLn $ "unexpected error while evaluating redeeming utxos: \n" ++ show e
                Right r -> putStrLn $ "Kontract redeeming utxos successfull: \n" ++ show r

