{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Buildy where

-- import Cardano.Api (AddressInEra, ConwayEra, deserialiseAddress, AsType (AsAddressInEra, AsConwayEra))
import Cardano.Api
import Cardano.Kuber.Api
import Cardano.Kuber.Util
import GHC.Base (join)

data Config = Config
  { cSndAddr :: AddressInEra ConwayEra
  , cSKey :: SigningKey PaymentKey
  , cRecAddr :: AddressInEra ConwayEra
  }

hardcodedSndAddress :: AddressInEra ConwayEra
hardcodedSndAddress = case deserialiseAddress (AsAddressInEra AsConwayEra) "addr_test1vz9vm2q6utqpw02s24vr5wdx3ry3n6sv39uppscylcc6kecpa9mur" of
  Just addr -> addr
  Nothing -> error "Invalid hardcoded addr"

hardcodedRecAddress :: AddressInEra ConwayEra
hardcodedRecAddress = case deserialiseAddress (AsAddressInEra AsConwayEra) "addr_test1qq9lealepm0l98t86dt9p6ct4g9xk4tn2rpqclzjp8fmk8zdzvrwtcjnte6qgwtutlqvzcet3jr5sfe6wphs6fw6g5zqcua95t" of
  Just addr -> addr
  Nothing -> error "Invalid hardcoded addr"

keyFromFilePath :: IO (SigningKey PaymentKey)
keyFromFilePath = readSignKey "payment.skey"

loadConfig :: IO Config
loadConfig = do
  -- signingKey <- readSignKey "payment.skey"
  key <- keyFromFilePath
  return
    Config
      { cSndAddr = hardcodedSndAddress
      , cSKey = key
      , cRecAddr = hardcodedRecAddress
      }

buildTHEKontract :: AddressInEra ConwayEra -> SigningKey PaymentKey -> TxBuilder
buildTHEKontract recipientAddr walletSKey =
  txPayTo recipientAddr (lovelaceToValue 2_000_000) <> txWalletSignKey walletSKey