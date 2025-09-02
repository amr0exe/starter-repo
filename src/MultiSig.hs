{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module MultiSig where

import PlutusLedgerApi.Common (BuiltinData, serialiseCompiledCode, FromData, fromBuiltinData)
import PlutusLedgerApi.V3 (PubKeyHash, scriptContextTxInfo, unsafeFromBuiltinData, Datum (getDatum))
import PlutusLedgerApi.V3.Contexts (TxInfo, ScriptContext, txInfoSignatories)
import PlutusTx (compile, CompiledCode, unstableMakeIsData)
import PlutusTx.List (filter, elem)
import PlutusTx.Foldable (length)
import Data.ByteString.Short (ShortByteString)
import Cardano.Api.Shelley (PlutusScript, PlutusScriptV3, PlutusScript(PlutusScriptSerialised))
import qualified PlutusTx.Builtins.Internal as BI
import PlutusTx.Prelude

data MsD = MsD
    { signatories :: [PubKeyHash]
    , min_num :: Integer
    }
unstableMakeIsData ''MsD


{-# INLINABLE constrArgs #-}
constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
constrArgs bd = BI.snd (BI.unsafeDataAsConstr bd)

{-# INLINABLE parseData #-}
parseData ::FromData a =>  BuiltinData -> BuiltinString -> a
parseData d s = case fromBuiltinData  d of
  Just d -> d
  _      -> traceError s

{-# INLINABLE mkValidator #-}
mkValidator :: MsD -> BuiltinData -> ScriptContext -> Bool
mkValidator params _ ctx = 
    traceIfFalse "not enough signatories" (countSigs >= req_no)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    req_sigs :: [PubKeyHash]
    req_sigs = signatories params

    req_no :: Integer
    req_no = min_num params

    crnt_sigs :: [PubKeyHash]
    crnt_sigs = txInfoSignatories info

    countSigs :: Integer
    countSigs = length (filter (`elem` req_sigs) crnt_sigs)

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinUnit
mkValidatorUntyped ctx = check (mkValidator dat rdm newSc)
    where
        context = constrArgs ctx -- turn to BuiltinData

        newSc = unsafeFromBuiltinData ctx

        redeemerFollowedByScriptInfo :: BI.BuiltinList BuiltinData
        redeemerFollowedByScriptInfo = BI.tail context

        redeemerBuiltinData :: BuiltinData
        redeemerBuiltinData = BI.head redeemerFollowedByScriptInfo

        scriptInfoData :: BuiltinData
        scriptInfoData = BI.head (BI.tail redeemerFollowedByScriptInfo)

        datumData :: BuiltinData
        datumData = BI.head (constrArgs (BI.head (BI.tail (constrArgs scriptInfoData))))

        rdm :: BuiltinData 
        rdm = parseData redeemerBuiltinData "Invalid redeemer Type"

        dat :: MsD
        dat = parseData (getDatum (unsafeFromBuiltinData datumData)) "Invalid Datum type"


cmpCode :: CompiledCode (BuiltinData -> BuiltinUnit)
cmpCode = $$(compile [|| mkValidatorUntyped ||]) 

validatorSerialized :: PlutusScript PlutusScriptV3 
validatorSerialized = PlutusScriptSerialised (serialiseCompiledCode cmpCode)