{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}


-- | A guessing game
module Example where

import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Ledger                (Address, Datum (Datum), ScriptContext, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada            as Ada
import           Ledger.Constraints    as Constraints
import           Ledger.Constraints    (TxConstraints (..))
import           Ledger.Tx             (ChainIndexTxOut (..))
import qualified Ledger.Typed.Scripts  as Scripts
import           Ledger.Value
import           Playground.Contract
import           Plutus.Contract
import qualified Plutus.Contract.Typed.Tx as Typed
import           Plutus.Contract.Trace as X
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
import           Plutus.Trace.Emulator (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Trace
import           Data.Text

type GameSchema =
        Endpoint "start" (Integer, Bool)

data Example
instance Scripts.ValidatorTypes Example where
    type instance RedeemerType Example = ()
    type instance DatumType Example = ()

exampleInstance :: Scripts.TypedValidator Example
exampleInstance = Scripts.mkTypedValidator @Example
    $$(PlutusTx.compile [|| \_ _ _ -> True ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @() @()

exampleValidator :: Validator
exampleValidator = Scripts.validatorScript exampleInstance

exampleAddress :: Address
exampleAddress = Ledger.scriptAddress exampleValidator

start :: Promise () GameSchema Text ()
start = endpoint @"start" $ \(adaAmount, putAnotherCoin) -> do
    let tx1      = Constraints.mustPayToTheScript () (assetClassValue (AssetClass ("ff", "some coin")) 100 <> assetClassValue (AssetClass ("","")) adaAmount)
    mkTxConstraints (Constraints.typedValidatorLookups exampleInstance) tx1 >>= void . submitUnbalancedTx. Constraints.adjustUnbalancedTx
    waitNSlots 1
    utxos <- utxosAt exampleAddress
    let anotherCoinTx = Constraints.mustPayToTheScript () (assetClassValue (AssetClass ("ee", "some other coin")) 200) 
    let tx2      = Typed.collectFromScript utxos () <> (if putAnotherCoin then anotherCoinTx else mempty)
    let lookups  = Constraints.typedValidatorLookups exampleInstance
                Haskell.<> Constraints.otherScript exampleValidator 
                Haskell.<> Constraints.unspentOutputs utxos
    mkTxConstraints lookups tx2 >>= void . submitUnbalancedTx. Constraints.adjustUnbalancedTx
    return ()
