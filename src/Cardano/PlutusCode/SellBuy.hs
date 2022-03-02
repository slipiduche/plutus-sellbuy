{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusCode.SellBuy
  ( sellBuySerialised
  , sellBuySBS
  ) where

--import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS

import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import           PlutusTx.Prelude as P hiding (Semigroup (..), unless)


{-
  The "hello world" message as a data item - converted to
  an Integer and shortened to fit within the 8-byte limit
  for an "int" datum.

  See SellBuyByteStringParametric.hs for an example of how to
  check a bytestring datume by passing a parameter to a validator.
-}

hello :: BuiltinData
hello = PlutusTx.toBuiltinData (0x48656c6c6f21 :: Integer)

{-
   The Hello World validator script
-}

{-# INLINABLE sellBuy #-}

-- sellBuy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- sellBuy datum _redeemer _context = if datum P.== hello then () else (P.error ())
sellBuy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
sellBuy datum redeemer context = if datum P.== redeemer then () else (P.error ())

{-
    As a Validator
-}

sellBuyValidator :: Plutus.Validator
sellBuyValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| sellBuy ||])

{-
    As a Script
-}

sellBuyScript :: Plutus.Script
sellBuyScript = Plutus.unValidatorScript sellBuyValidator

{-
    As a Short Byte String
-}

sellBuySBS :: SBS.ShortByteString
sellBuySBS =  SBS.toShort . LBS.toStrict $ serialise sellBuyScript

{-
    As a Serialised Script
-}

sellBuySerialised :: PlutusScript PlutusScriptV1
sellBuySerialised = PlutusScriptSerialised sellBuySBS
