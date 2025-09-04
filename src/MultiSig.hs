{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MultiSig where

import GHC.Generics (Generic)
import Prelude (Show)

import PlutusTx (compile, unstableMakeIsData, makeLift, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCode)

import PlutusLedgerApi.Common (BuiltinData, serialiseCompiledCode, FromData, fromBuiltinData)
import PlutusLedgerApi.V3 (PubKeyHash, scriptContextTxInfo, unsafeFromBuiltinData)
import PlutusLedgerApi.V3.Contexts (TxInfo, ScriptContext, txInfoSignatories)

import qualified PlutusTx.Builtins.Internal as BI
import PlutusTx.Prelude
import PlutusTx.List (filter, elem)
import PlutusTx.Foldable (length)

import Cardano.Api.Shelley (PlutusScript, PlutusScriptV3, PlutusScript(PlutusScriptSerialised))
import PlutusCore.Version (plcVersion110)

data MsD = MsD
    { signatories :: [PubKeyHash]
    , min_num     :: Integer
    } deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''MsD
PlutusTx.makeLift ''MsD

{-# INLINABLE constrArgs #-}
constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
constrArgs bd = BI.snd (BI.unsafeDataAsConstr bd)

{-# INLINABLE parseData #-}
parseData ::FromData a =>  BuiltinData -> BuiltinString -> a
parseData d s = case fromBuiltinData  d of
  Just d -> d
  _      -> traceError s

{-# INLINABLE mkValidator #-}
mkValidator :: MsD -> () -> () -> ScriptContext -> Bool
mkValidator params _ _ ctx =
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
mkValidatorUntyped :: MsD -> BuiltinData -> BuiltinUnit
mkValidatorUntyped pMSD ctx = check (mkValidator pMSD () () newSc)
    where
        newSc = unsafeFromBuiltinData ctx

cmpCode :: MsD -> CompiledCode(BuiltinData -> BuiltinUnit)
cmpCode param =
    $$(compile [|| mkValidatorUntyped ||])
        `unsafeApplyCode` liftCode plcVersion110 param


validatorSerialized :: MsD -> PlutusScript PlutusScriptV3
validatorSerialized param = PlutusScriptSerialised (serialiseCompiledCode (cmpCode param))
