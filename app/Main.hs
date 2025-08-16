module Main where

import Prelude
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import InContract (validatorSerialized)

main :: IO ()
main = do
    let bytes = SBS.fromShort validatorSerialized

    -- write CBOR file
    BS.writeFile "InContract.plutus" bytes

    putStrLn $ "CBOR hex:: " ++ show (B16.encode bytes)
