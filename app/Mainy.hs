module Mainy where

import Cardano.Api
import Cardano.Kuber.Api
import Buildy (loadConfig, buildTHEKontract, Config (cRecAddr, cSKey))

remoteKuberConnection :: IO RemoteKuberConnection
remoteKuberConnection = do
    (networkName,network) <- getNetworkFromEnv "NETWORK"
    createRemoteKuberConnection network "http://localhost:8081" Nothing

localNodeConnection :: IO ChainConnectInfo
localNodeConnection = chainInfoFromEnv

main :: IO ()
main = do
    config <- loadConfig
    kuberConn <- localNodeConnection

    let txBuilder = buildTHEKontract (cRecAddr config) (cSKey config)
    
    buildResult <- evaluateKontract kuberConn (kBuildTx txBuilder)
    case buildResult of
        Left buildErr -> putStrLn $ "Transaction Build Failed:: " ++ show buildErr
        Right tx -> do
            putStrLn "Transaction built successfully"
            submitResult <- evaluateKontract kuberConn (kSubmitTx (InAnyCardanoEra ConwayEra tx))
            case submitResult of
                Left submitErr -> putStrLn $ "Submit failed: " ++ show submitErr
                Right _ -> putStrLn "Transaction submitted successfully"
    {-
    result <- evaluateKontract kuberConn (kBuildAndSubmit txBuilder)
    case result of
        Left e -> putStrLn $ "Unexpected error while evaluating contract: \n" ++ show e
        Right r -> putStrLn $ "Kontract execution successfull: \n" ++ show r
-}
