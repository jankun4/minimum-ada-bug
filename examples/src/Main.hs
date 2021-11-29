{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE LambdaCase         #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Main 
  where

import           Data.Default
import           Data.Functor                  (void)
import           Wallet.Emulator.Wallet        as Wallet
import           Ledger.Value                            as Value
import qualified Ledger.Ada                              as Ada
import qualified Data.Map as Map
import           Plutus.Trace.Emulator.Types             (ContractInstanceLog (..), ContractInstanceMsg (..))
import           Wallet.Emulator.MultiAgent              (EmulatorEvent' (..))

import qualified Data.Aeson                              as A
import           Data.Text.Prettyprint.Doc               (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.String (renderString)
import           System.IO                               (stdout)
import           Example
import           System.Environment
import           Plutus.Trace.Emulator
customSymbolsAndTokens :: [(Value.CurrencySymbol, Value.TokenName)]
customSymbolsAndTokens = [("ff", "some coin"), ("ee", "some other coin")] 


main :: IO ()
main = do
  args <- getArgs
  case args of
    [adaAmount, "put"] -> runTrace (exampleTrace (read adaAmount) True)
    (adaAmount:_)      -> runTrace (exampleTrace (read adaAmount) False)
    _ -> putStrLn "wrong args"

emulatorCfg :: EmulatorConfig
emulatorCfg = EmulatorConfig (Left $ Map.fromList ([(knownWallet i, v) | i <- [1 .. 1]])) def def
      where
        v = Ada.lovelaceValueOf 100_000_000 <> mconcat (map (\(symbol,tokenName') -> Value.singleton symbol tokenName' 100_000_000) customSymbolsAndTokens)

runTrace :: EmulatorTrace () -> IO ()
runTrace = runEmulatorTraceIO' customTraceConfig emulatorCfg

defaultShowEvent :: EmulatorEvent' -> Maybe String
defaultShowEvent = \case
  UserThreadEvent msg                                                  -> Just $ "*** USER LOG: " <> render msg
  InstanceEvent (ContractInstanceLog (ContractLog (A.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> show err
  InstanceEvent (ContractInstanceLog NoRequestsHandled            _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _)           _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _)          _ _) -> Nothing
  SchedulerEvent _                                                     -> Nothing
  ChainIndexEvent _ _                                                  -> Nothing
  ev                                                                   -> Just . render $ ev
  where
      render :: Pretty a => a -> String
      render = renderString . layoutPretty defaultLayoutOptions . pretty

customTraceConfig :: TraceConfig
customTraceConfig =
  TraceConfig
    { showEvent = defaultShowEvent,
      outputHandle = stdout
    }

exampleTrace :: Integer -> Bool -> EmulatorTrace ()
exampleTrace adaAmount putAnother = do
  h1 <- activateContractWallet (knownWallet 1) start
  void $ callEndpoint @"start" h1 (adaAmount, putAnother)
  _ <- waitNSlots 3
  return ()
