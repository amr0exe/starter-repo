module Rmy.LockAtSc where

import Cardano.Api
import Cardano.Kuber.Api
import Cardano.Kuber.Util
import PlutusTx (toData)
import PlutusLedgerApi.Common
import PlutusLedgerApi.V3 (PubKeyHash(..))
import MultiSig (MsD(..))

lockAtScript
    :: AddressInEra ConwayEra
    -> Value
    -> SigningKey PaymentKey
    -> Kontract ChainConnectInfo w FrameworkError TxBuilder
lockAtScript scriptAddr value walletSkey = do
    let datumForScript = unsafeHashableScriptData $ fromPlutusData $ toData ()
    let txBuilder = txPayToScriptWithData scriptAddr value datumForScript <> txWalletSignKey walletSkey
    pure txBuilder

