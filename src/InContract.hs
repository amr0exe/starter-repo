{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module InContract where

import PlutusLedgerApi.Common (BuiltinData, serialiseCompiledCode, SerialisedScript)
import PlutusTx (compile, CompiledCode)
import PlutusTx.Prelude
import Data.ByteString.Short (ShortByteString)


{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [|| mkValidator ||])

validatorSerialized :: ShortByteString
validatorSerialized = serialiseCompiledCode validatorCode
