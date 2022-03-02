{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}



import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
import           Schema               (ToSchema)
import           Text.Printf          (printf)
----------------------------------------------------------------------------
--import           Prelude hiding (($))
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import qualified Plutus.V1.Ledger.Scripts as Plutus

--import           PlutusTx.Prelude as P hiding (Semigroup (..), unless)

minLovelace :: Integer
minLovelace = 2000000

data SellOffer = SellOffer
    { aSeller   :: !PaymentPubKeyHash
    , aSellPrice   :: !Integer
    , aCurrency :: !CurrencySymbol
    , aToken    :: !TokenName
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq SellOffer where
    {-# INLINABLE (==) #-}
    a == b = (aSeller   a == aSeller   b) &&
             (aSellPrice   a == aSellPrice   b) &&
             (aCurrency a == aCurrency b) &&
             (aToken    a == aToken    b)

PlutusTx.unstableMakeIsData ''SellOffer
PlutusTx.makeLift ''SellOffer

data BuyOffer = BuyOffer
    { bBuyer :: !PaymentPubKeyHash
    , bBuyOffer    :: !Integer
    } deriving P.Show

instance Eq BuyOffer where
    {-# INLINABLE (==) #-}
    b == c = (bBuyer b == bBuyer c) &&
             (bBuyOffer    b == bBuyOffer    c)

PlutusTx.unstableMakeIsData ''BuyOffer
PlutusTx.makeLift ''BuyOffer

data SellOfferAction = MkBuyOffer BuyOffer | Close
    deriving P.Show

PlutusTx.unstableMakeIsData ''SellOfferAction
PlutusTx.makeLift ''SellOfferAction

data SellOfferDatum = SellOfferDatum
    { adSellOffer    :: !SellOffer
    , adBuyOffer :: !(Maybe BuyOffer)
    } deriving P.Show

PlutusTx.unstableMakeIsData ''SellOfferDatum
PlutusTx.makeLift ''SellOfferDatum

data SellOffering
instance Scripts.ValidatorTypes SellOffering where
    type instance RedeemerType SellOffering = SellOfferAction
    type instance DatumType SellOffering = SellOfferDatum

{-# INLINABLE minBuyOffer #-}
minBuyOffer :: SellOfferDatum -> Integer
minBuyOffer SellOfferDatum{..} = aSellPrice adSellOffer
    

{-# INLINABLE mkSellOfferValidator #-}
mkSellOfferValidator :: SellOfferDatum -> SellOfferAction -> ScriptContext -> Bool
mkSellOfferValidator ad redeemer ctx =
    traceIfFalse "wrong input value" correctInputValue &&
    case redeemer of
        MkBuyOffer b@BuyOffer{..} ->
            traceIfFalse "buy offer too low"        (sufficientBuyOffer bBuyOffer)   &&
            traceIfFalse "wrong output datum" (correctBuyOfferOutputDatum b)         &&
            traceIfFalse "wrong output value" (correctBuyOfferOutputValue bBuyOffer)   
        Close           -> 
            case adBuyOffer ad of
                Nothing      ->
                    traceIfFalse "expected seller to get token" (getsValue (aSeller sellOffer) $ tokenValue <> Ada.lovelaceValueOf minLovelace)
                Just BuyOffer{..} ->
                    traceIfFalse "expected highest buyer to get token" (getsValue bBuyer $ tokenValue <> Ada.lovelaceValueOf minLovelace) &&
                    traceIfFalse "expected seller to get highest buyoffer" (getsValue (aSeller sellOffer) $ Ada.lovelaceValueOf bBuyOffer)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs of
            [i] -> i
            _   -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    sellOffer :: SellOffer
    sellOffer = adSellOffer ad

    tokenValue :: Value
    tokenValue = Value.singleton (aCurrency sellOffer) (aToken sellOffer) 1

    correctInputValue :: Bool
    correctInputValue = inVal == case adBuyOffer ad of
        Nothing      -> tokenValue <> Ada.lovelaceValueOf minLovelace
        Just BuyOffer{..} -> tokenValue <> Ada.lovelaceValueOf (minLovelace + bBuyOffer)

    sufficientBuyOffer :: Integer -> Bool
    sufficientBuyOffer amount = amount >= minBuyOffer ad

    ownOutput   :: TxOut
    outputDatum :: SellOfferDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctBuyOfferOutputDatum :: BuyOffer -> Bool
    correctBuyOfferOutputDatum b = (adSellOffer outputDatum == sellOffer)   &&
                              (adBuyOffer outputDatum == Just b)

    correctBuyOfferOutputValue :: Integer -> Bool
    correctBuyOfferOutputValue amount =
        txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf (minLovelace + amount)

    -- correctBuyOfferRefund :: Bool
    -- correctBuyOfferRefund = case adHighestBuyOffer ad of
    --     Nothing      -> True
    --     Just BuyOffer{..} ->
    --       let
    --         os = [ o
    --              | o <- txInfoOutputs info
    --              , txOutAddress o == pubKeyHashAddress bBuyer Nothing
    --              ]
    --       in
    --         case os of
    --             [o] -> txOutValue o == Ada.lovelaceValueOf bBuyOffer
    --             _   -> traceError "expected exactly one refund output"

    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing

typedSellOfferValidator :: Scripts.TypedValidator SellOffering
typedSellOfferValidator = Scripts.mkTypedValidator @SellOffering
    $$(PlutusTx.compile [|| mkSellOfferValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SellOfferDatum @SellOfferAction

sellOfferValidator :: Validator
sellOfferValidator = Scripts.validatorScript typedSellOfferValidator

------

{-
    As a Script
-}

sellBuyScript :: Plutus.Script
sellBuyScript = Plutus.unValidatorScript sellOfferValidator

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

sellOfferHash :: Ledger.ValidatorHash
sellOfferHash = Scripts.validatorHash typedSellOfferValidator

sellOfferAddress :: Ledger.Address
sellOfferAddress = scriptHashAddress sellOfferHash

data SellParams = SellParams
    { spSellPrice   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data BuyOfferParams = BuyOfferParams
    { bpCurrency :: !CurrencySymbol
    , bpToken    :: !TokenName
    , bpBuyOffer      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams
    { cpCurrency :: !CurrencySymbol
    , cpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SellOfferSchema =
        Endpoint "sell" SellParams
    .\/ Endpoint "buy"   BuyOfferParams
    .\/ Endpoint "close" CloseParams

sell :: AsContractError e => SellParams -> Contract w s e ()
sell SellParams{..} = do
    pkh <- ownPaymentPubKeyHash
    let a = SellOffer
                { aSeller   = pkh
                , aSellPrice   = spSellPrice
                , aCurrency = spCurrency
                , aToken    = spToken
                }
        d = SellOfferDatum
                { adSellOffer    = a
                 , adBuyOffer = Nothing
                }
        v = Value.singleton spCurrency spToken 1 <> Ada.lovelaceValueOf minLovelace
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints typedSellOfferValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "started sellOffer %s for token %s" (P.show a) (P.show v)

buy :: forall w s. BuyOfferParams -> Contract w s Text ()
buy BuyOfferParams{..} = do
    (oref, o, d@SellOfferDatum{..}) <- findSellOffer bpCurrency bpToken
    logInfo @P.String $ printf "found sellOffer utxo with datum %s" (P.show d)

    when (bpBuyOffer < minBuyOffer d) $
        throwError $ pack $ printf "buy offer lower than minimal buy offer %d" (minBuyOffer d)
    pkh <- ownPaymentPubKeyHash
    let b  = BuyOffer {bBuyer = pkh, bBuyOffer = bpBuyOffer}
        d' = d {adBuyOffer = Just b}
        r  = Redeemer $ PlutusTx.toBuiltinData $ MkBuyOffer b
        t      = Value.singleton bpCurrency bpToken 1
        seller = aSeller adSellOffer
        buyer=pkh
        buyOffer=bpBuyOffer
        
        lookups = Constraints.typedValidatorLookups typedSellOfferValidator P.<>
                  Constraints.otherScript sellOfferValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = 
                  Constraints.mustPayToPubKey buyer (t <> Ada.lovelaceValueOf minLovelace) <>
                  Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bpBuyOffer)        <>
                  Constraints.mustSpendScriptOutput oref r
     
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made buy offer of %d lovelace in sellOffer %s for token (%s, %s)"
        bpBuyOffer
        (P.show adSellOffer)
        (P.show bpCurrency)
        (P.show bpToken)

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    (oref, o, d@SellOfferDatum{..}) <- findSellOffer cpCurrency cpToken
    logInfo @P.String $ printf "found sellOffer utxo with datum %s" (P.show d)

    let t      = Value.singleton cpCurrency cpToken 1
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adSellOffer

        lookups = Constraints.typedValidatorLookups typedSellOfferValidator P.<>
                  Constraints.otherScript sellOfferValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adBuyOffer of
                    Nothing      -> Constraints.mustPayToPubKey seller (t <> Ada.lovelaceValueOf minLovelace)  <>
                                    Constraints.mustSpendScriptOutput oref r
                    Just BuyOffer{..} -> Constraints.mustPayToPubKey bBuyer (t <> Ada.lovelaceValueOf minLovelace) <>
                                    Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bBuyOffer)              <>
                                    Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "closed sellOffer %s for token (%s, %s)"
        (P.show adSellOffer)
        (P.show cpCurrency)
        (P.show cpToken)

findSellOffer :: CurrencySymbol
            -> TokenName
            -> Contract w s Text (TxOutRef, ChainIndexTxOut, SellOfferDatum)
findSellOffer cs tn = do
    utxos <- utxosAt $ scriptHashAddress sellOfferHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (_ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@SellOfferDatum{..}
                    | aCurrency adSellOffer == cs && aToken adSellOffer == tn -> return (oref, o, d)
                    | otherwise                                           -> throwError "sellOffer token missmatch"
        _           -> throwError "sellOffer utxo not found"

endpoints :: Contract () SellOfferSchema Text ()
endpoints = awaitPromise (sell' `select` buy' `select` close') >> endpoints
  where
    sell' = endpoint @"sell" sell
    buy'   = endpoint @"buy"   buy
    close' = endpoint @"close" close

mkSchemaDefinitions ''SellOfferSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "NFT F66" :| [])


myTokenG :: KnownCurrency
myTokenG = KnownCurrency (ValidatorHash "g") "Token" (TokenName "NFT G67" :| [])
mkKnownCurrencies ['myToken,'myTokenG]


