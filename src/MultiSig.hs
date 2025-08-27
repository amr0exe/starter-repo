{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module MultiSig where

import PlutusLedgerApi.Common (BuiltinData, serialiseCompiledCode)
import PlutusLedgerApi.V3.Contexts (TxInfo, ScriptContext, txInfoSignatories)
import PlutusTx.Prelude (Bool, traceIfFalse, BuiltinUnit, error, traceError)
import PlutusLedgerApi.V3 (PubKeyHash, scriptContextTxInfo, unsafeFromBuiltinData)

import PlutusTx.Ord (Ord((>=)))
import PlutusTx.Builtins (Integer, greaterThanEqualsInteger)
import PlutusTx (compile, CompiledCode, unstableMakeIsData)
import PlutusTx.List (filter, elem)
import PlutusTx.Foldable (length)
import Data.ByteString.Short (ShortByteString)

data MsD = MsD
    { signatories :: [PubKeyHash]
    , min_num :: Integer
    }
unstableMakeIsData ''MsD

{-# INLINABLE mkValidator #-}
mkValidator :: MsD -> () -> ScriptContext -> ()
mkValidator params _ ctx =
        if countSigs >= req_no
            then ()
            else traceError "not enough signatories"
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
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped params redeemer ctx = 
    mkValidator
        (unsafeFromBuiltinData params)
        (unsafeFromBuiltinData redeemer :: ())
        (unsafeFromBuiltinData ctx)

cmpCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
cmpCode = $$(compile [|| mkValidatorUntyped ||])

validatorSerialized = serialiseCompiledCode cmpCode