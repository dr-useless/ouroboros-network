{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Query (
    Query(..)
  , QueryIfCurrent(..)
  , HardForkQueryResult
  , QueryAnytime(..)
  , QueryHardFork(..)
  , getHardForkQuery
  , hardForkQueryInfo
  , encodeQueryAnytimeResult
  , decodeQueryAnytimeResult
  , encodeQueryHardForkResult
  , decodeQueryHardForkResult
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.Bifunctor
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Type.Equality
import           Data.Typeable (Typeable)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract (hardForkSummary)
import           Ouroboros.Consensus.HardFork.History (Bound (..),
                     EraParams (..), Shape (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Node.Serialisation (Some (..))
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapChainDepState (..))
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.Counting (getExactly)

import           Ouroboros.Consensus.BlockchainTime (SlotLength)
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..),
                     Past (..), Situated (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match
                     (Mismatch (..), mustMatchNS)

instance Typeable xs => ShowProxy (Query (HardForkBlock xs)) where

instance All SingleEraBlock xs => ShowQuery (Query (HardForkBlock xs)) where
  showResult (QueryAnytime   qry _) result = showResult qry result
  showResult (QueryHardFork  qry)   result = showResult qry result
  showResult (QueryIfCurrent qry)  mResult =
      case mResult of
        Left  err    -> show err
        Right result -> showResult qry result

type HardForkQueryResult xs = Either (MismatchEraInfo xs)

data instance Query (HardForkBlock xs) :: Type -> Type where
  -- | Answer a query about an era if it is the current one.
  QueryIfCurrent ::
       QueryIfCurrent xs result
    -> Query (HardForkBlock xs) (HardForkQueryResult xs result)

  -- | Answer a query about an era from /any/ era.
  --
  -- NOTE: we don't allow this when there is only a single era, so that the
  -- HFC applied to a single era is still isomorphic to the single era.
  QueryAnytime ::
       IsNonEmpty xs
    => QueryAnytime result
    -> EraIndex (x ': xs)
    -> Query (HardForkBlock (x ': xs)) result

  -- | Answer a query about the hard fork combinator
  --
  -- NOTE: we don't allow this when there is only a single era, so that the
  -- HFC applied to a single era is still isomorphic to the single era.
  QueryHardFork ::
       IsNonEmpty xs
    => QueryHardFork (x ': xs) result
    -> Query (HardForkBlock (x ': xs)) result

  -- WARNING: please add new queries to the end of the list here and for nested
  -- query types. Stick to this order in all other pattern matches on queries.
  -- This helps in particular with the en/decoders, as we want the CBOR tags to
  -- be ordered.
  --
  -- WARNING: when adding a new query, a new
  -- @HardForkSpecificNodeToClientVersionX@ must be added. See #2973 for a
  -- template on how to do this.
  --
  -- WARNING: never modify an existing query that has been incorporated in a
  -- release of the node, as it will break compatibility with deployed nodes.
  -- Instead, add a new query. To remove the old query, first to stop supporting
  -- it by modifying
  -- 'Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToClient.querySupportedVersion'
  -- (@< X@) and when the version is no longer used (because mainnet has
  -- hard-forked to a newer version), it can be removed.

instance All SingleEraBlock xs => QueryLedger (HardForkBlock xs) where
  answerQuery (ExtLedgerCfg cfg)
              query
              ext@(ExtLedgerState st@(HardForkLedgerState hardForkState) _) =
      case query of
        QueryIfCurrent queryIfCurrent ->
          interpretQueryIfCurrent
            cfgs
            queryIfCurrent
            (distribExtLedgerState ext)
        QueryAnytime queryAnytime (EraIndex era) ->
          interpretQueryAnytime
            lcfg
            queryAnytime
            (EraIndex era)
            hardForkState
        QueryHardFork queryHardFork ->
          interpretQueryHardFork
            cfg
            queryHardFork
            st
    where
      cfgs = hmap ExtLedgerCfg $ distribTopLevelConfig ei cfg
      lcfg = configLedger cfg
      ei   = State.epochInfoLedger lcfg hardForkState

-- | Precondition: the 'ledgerState' and 'headerState' should be from the same
-- era. In practice, this is _always_ the case, unless the 'ExtLedgerState' was
-- manually crafted.
distribExtLedgerState ::
     All SingleEraBlock xs
  => ExtLedgerState (HardForkBlock xs) -> NS ExtLedgerState xs
distribExtLedgerState (ExtLedgerState ledgerState headerState) =
    hmap (\(Pair hst lst) -> ExtLedgerState lst hst) $
      mustMatchNS
        "HeaderState"
        (distribHeaderState headerState)
        (State.tip (hardForkLedgerStatePerEra ledgerState))

-- | Precondition: the 'headerStateTip' and 'headerStateChainDep' should be from
-- the same era. In practice, this is _always_ the case, unless the
-- 'HeaderState' was manually crafted.
distribHeaderState ::
     All SingleEraBlock xs
  => HeaderState (HardForkBlock xs) -> NS HeaderState xs
distribHeaderState (HeaderState tip chainDepState) =
    case tip of
      Origin ->
        hmap (HeaderState Origin . unwrapChainDepState) (State.tip chainDepState)
      NotOrigin annTip ->
        hmap
          (\(Pair t cds) -> HeaderState (NotOrigin t) (unwrapChainDepState cds))
          (mustMatchNS "AnnTip" (distribAnnTip annTip) (State.tip chainDepState))

instance All SingleEraBlock xs => SameDepIndex (Query (HardForkBlock xs)) where
  sameDepIndex (QueryIfCurrent qry) (QueryIfCurrent qry') =
      apply Refl <$> sameDepIndex qry qry'
  sameDepIndex (QueryIfCurrent {}) _ =
      Nothing
  sameDepIndex (QueryAnytime qry era) (QueryAnytime qry' era')
    | era == era'
    = sameDepIndex qry qry'
    | otherwise
    = Nothing
  sameDepIndex (QueryAnytime {}) _ =
      Nothing
  sameDepIndex (QueryHardFork qry) (QueryHardFork qry') =
      sameDepIndex qry qry'
  sameDepIndex (QueryHardFork {}) _ =
      Nothing

deriving instance All SingleEraBlock xs => Show (Query (HardForkBlock xs) result)

getHardForkQuery :: Query (HardForkBlock xs) result
                 -> (forall result'.
                          result :~: HardForkQueryResult xs result'
                       -> QueryIfCurrent xs result'
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> ProofNonEmpty xs'
                       -> QueryAnytime result
                       -> EraIndex xs
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> ProofNonEmpty xs'
                       -> QueryHardFork xs result
                       -> r)
                 -> r
getHardForkQuery q k1 k2 k3 = case q of
    QueryIfCurrent qry   -> k1 Refl qry
    QueryAnytime qry era -> k2 Refl (isNonEmpty Proxy) qry era
    QueryHardFork qry    -> k3 Refl (isNonEmpty Proxy) qry

{-------------------------------------------------------------------------------
  Current era queries
-------------------------------------------------------------------------------}

data QueryIfCurrent :: [Type] -> Type -> Type where
  QZ :: Query x result           -> QueryIfCurrent (x ': xs) result
  QS :: QueryIfCurrent xs result -> QueryIfCurrent (x ': xs) result

deriving instance All SingleEraBlock xs => Show (QueryIfCurrent xs result)

instance All SingleEraBlock xs => ShowQuery (QueryIfCurrent xs) where
  showResult (QZ qry) = showResult qry
  showResult (QS qry) = showResult qry

instance All SingleEraBlock xs => SameDepIndex (QueryIfCurrent xs) where
  sameDepIndex (QZ qry) (QZ qry') = sameDepIndex qry qry'
  sameDepIndex (QS qry) (QS qry') = sameDepIndex qry qry'
  sameDepIndex _        _         = Nothing

interpretQueryIfCurrent ::
     forall result xs. All SingleEraBlock xs
  => NP ExtLedgerCfg xs
  -> QueryIfCurrent xs result
  -> NS ExtLedgerState xs
  -> HardForkQueryResult xs result
interpretQueryIfCurrent = go
  where
    go :: All SingleEraBlock xs'
       => NP ExtLedgerCfg xs'
       -> QueryIfCurrent xs' result
       -> NS ExtLedgerState xs'
       -> HardForkQueryResult xs' result
    go (c :* _)  (QZ qry) (Z st) =
        Right $ answerQuery c qry st
    go (_ :* cs) (QS qry) (S st) =
        first shiftMismatch $ go cs qry st
    go _         (QZ qry) (S st) =
        Left $ MismatchEraInfo $ ML (queryInfo qry) (hcmap proxySingle ledgerInfo st)
    go _         (QS qry) (Z st) =
        Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

{-------------------------------------------------------------------------------
  Any era queries
-------------------------------------------------------------------------------}

data QueryAnytime result where
  GetEraStart   :: QueryAnytime (Maybe Bound)
  GetSlotLength :: QueryAnytime SlotLength
  GetEpochSize  :: QueryAnytime EpochSize

deriving instance Show (QueryAnytime result)

instance ShowQuery QueryAnytime where
  showResult GetEraStart   = show
  showResult GetSlotLength = show
  showResult GetEpochSize  = show

instance SameDepIndex QueryAnytime where
  sameDepIndex GetEraStart GetEraStart     = Just Refl
  sameDepIndex GetEraStart _               = Nothing
  sameDepIndex GetSlotLength GetSlotLength = Just Refl
  sameDepIndex GetSlotLength _             = Nothing
  sameDepIndex GetEpochSize GetEpochSize   = Just Refl
  sameDepIndex GetEpochSize _              = Nothing

interpretQueryAnytime ::
     forall result xs. All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime result
  -> EraIndex xs
  -> State.HardForkState LedgerState xs
  -> result
interpretQueryAnytime HardForkLedgerConfig{..} query (EraIndex era) st
  = case query of
    GetEraStart ->
      goGetEraStart
        allLedgerConfigs
        allEraParams
        (State.situate era st)
    GetSlotLength -> eraSlotLength $ lookupEraParams (EraIndex era) allEraParams
    GetEpochSize -> eraEpochSize $ lookupEraParams (EraIndex era) allEraParams
  where
    allLedgerConfigs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    allEraParams = getExactly (getShape hardForkLedgerConfigShape)

    goGetEraStart :: All SingleEraBlock xs'
       => NP WrapPartialLedgerConfig xs'
       -> NP (K EraParams) xs'
       -> Situated h LedgerState xs'
       -> (Maybe Bound)
    goGetEraStart Nil       _             ctxt = case ctxt of {}
    goGetEraStart (c :* cs) (K ps :* pss) ctxt = case ctxt of
      SituatedShift ctxt'   -> goGetEraStart cs pss ctxt'
      SituatedFuture _ _    -> Nothing
      SituatedPast past _   -> Just $ pastStart $ unK past
      SituatedCurrent cur _ -> Just $ currentStart cur
      SituatedNext cur _    ->
        History.mkUpperBound ps (currentStart cur) <$>
          singleEraTransition
          (unwrapPartialLedgerConfig c)
          ps
          (currentStart cur)
          (currentState cur)

    -- | Lookup the EraParams at the given EraIndex and return the slot
    -- length.
    lookupEraParams :: All SingleEraBlock xs'
       => EraIndex xs'
       -> NP (K EraParams) xs'
       -> EraParams
    lookupEraParams (EraIndex era') (K eraParams :* eraParamsRest)
      = case era' of
        Z _     -> eraParams
        S era'' -> lookupEraParams (EraIndex era'') eraParamsRest

{-------------------------------------------------------------------------------
  Hard fork queries
-------------------------------------------------------------------------------}

data QueryHardFork xs result where
  GetInterpreter   :: QueryHardFork xs (History.Interpreter xs)
  GetCurrentEra    :: QueryHardFork xs (EraIndex xs)
  GetLedgerCfg     :: QueryHardFork xs (HardForkLedgerConfig xs)
  GetSecurityParam :: QueryHardFork xs SecurityParam

deriving instance Show (QueryHardFork xs result)

instance All SingleEraBlock xs => ShowQuery (QueryHardFork xs) where
  showResult query = case query of
    GetInterpreter   -> show
    GetCurrentEra    -> show
    -- As the HardForkLedgerConfig is quite large and we want to avoid excessive
    -- debug output, we truncate the contents of HardForkLedgerConfig here.
    GetLedgerCfg     -> \HardForkLedgerConfig{} -> "HardForkLedgerConfig{..}"
    GetSecurityParam -> show

instance SameDepIndex (QueryHardFork xs) where
  sameDepIndex GetInterpreter GetInterpreter =
      Just Refl
  sameDepIndex GetInterpreter _ =
      Nothing
  sameDepIndex GetCurrentEra GetCurrentEra =
      Just Refl
  sameDepIndex GetCurrentEra _ =
      Nothing
  sameDepIndex GetLedgerCfg GetLedgerCfg =
      Just Refl
  sameDepIndex GetLedgerCfg _ =
      Nothing
  sameDepIndex GetSecurityParam GetSecurityParam =
      Just Refl
  sameDepIndex GetSecurityParam _ =
      Nothing

interpretQueryHardFork ::
     All SingleEraBlock xs
  => TopLevelConfig (HardForkBlock xs)
  -> QueryHardFork xs result
  -> LedgerState (HardForkBlock xs)
  -> result
interpretQueryHardFork cfg query st =
    case query of
      GetInterpreter ->
        History.mkInterpreter $ hardForkSummary (configLedger cfg) st
      GetCurrentEra  ->
        eraIndexFromNS $ State.tip $ hardForkLedgerStatePerEra st
      GetLedgerCfg -> configLedger cfg
      GetSecurityParam -> hardForkConsensusConfigK (configConsensus cfg)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise (Some QueryAnytime) where
  encode = \case
    Some GetEraStart -> mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 0
      ]
    Some GetSlotLength -> mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 1
      ]
    Some GetEpochSize -> mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 2
      ]

  decode = do
    enforceSize "QueryAnytime" 1
    tag <- Dec.decodeWord8
    case tag of
      0 -> return $ Some GetEraStart
      1 -> return $ Some GetSlotLength
      2 -> return $ Some GetEpochSize
      _ -> fail $ "QueryAnytime: invalid tag " ++ show tag

encodeQueryAnytimeResult :: QueryAnytime result -> result -> Encoding
encodeQueryAnytimeResult = \case
   GetEraStart   -> encode
   GetSlotLength -> toCBOR
   GetEpochSize  -> toCBOR

decodeQueryAnytimeResult :: QueryAnytime result -> forall s. Decoder s result
decodeQueryAnytimeResult = \case
   GetEraStart   -> decode
   GetSlotLength -> fromCBOR
   GetEpochSize  -> fromCBOR

encodeQueryHardForkResult ::
     (ToCBOR (LedgerConfig (HardForkBlock xs)), SListI xs)
  => QueryHardFork xs result -> result -> Encoding
encodeQueryHardForkResult = \case
    GetInterpreter   -> encode
    GetCurrentEra    -> encode
    GetLedgerCfg     -> toCBOR
    GetSecurityParam -> toCBOR

decodeQueryHardForkResult ::
     (FromCBOR (LedgerConfig (HardForkBlock xs)), SListI xs)
  => QueryHardFork xs result -> forall s. Decoder s result
decodeQueryHardForkResult = \case
    GetInterpreter   -> decode
    GetCurrentEra    -> decode
    GetLedgerCfg     -> fromCBOR
    GetSecurityParam -> fromCBOR

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => ExtLedgerState blk
           -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

queryInfo :: forall blk query result. SingleEraBlock blk
          => query blk result -> SingleEraInfo blk
queryInfo _ = singleEraInfo (Proxy @blk)

hardForkQueryInfo :: All SingleEraBlock xs
                  => QueryIfCurrent xs result -> NS SingleEraInfo xs
hardForkQueryInfo = go
  where
    go :: All SingleEraBlock xs'
       => QueryIfCurrent xs' result -> NS SingleEraInfo xs'
    go (QZ qry) = Z (queryInfo qry)
    go (QS qry) = S (go qry)

shiftMismatch :: MismatchEraInfo xs -> MismatchEraInfo (x ': xs)
shiftMismatch = MismatchEraInfo . MS . getMismatchEraInfo
