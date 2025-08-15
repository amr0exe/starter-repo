{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module InContract where

import PlutusLedgerApi.Common (BuiltinData, serialiseCompiledCode)
import PlutusTx (CompiledCode, compile)
import PlutusTx.Blueprint (CompiledValidator)

import System.IO (writeFile, IO, putStrLn)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import PlutusTx.Base (($))
import Cardano.Api.Shelley (PlutusScript(PlutusScriptSerialised), PlutusScriptV3, Script (PlutusScript), PlutusScriptVersion (PlutusScriptV3), writeFileTextEnvelope, SerialiseAsCBOR (serialiseToCBOR))
import Control.Exception (Exception(displayException))
import qualified Data.ByteString as BSL
import PlutusTx.Prelude ((.), check, BuiltinUnit)
import qualified Codec.Serialise as B16

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [|| mkValidator ||])

wrapped :: PlutusScript PlutusScriptV3
wrapped = PlutusScriptSerialised . BSL.toStrict $ serialiseToCBOR validatorCode

main :: IO ()
main = do
    let cborBytes = serialiseToCBOR validatorCode
    let cborHex = B16.encode $ BSL.toStrict cborBytes
    BSL.writeFile "InContract.plutus" cborBytes
