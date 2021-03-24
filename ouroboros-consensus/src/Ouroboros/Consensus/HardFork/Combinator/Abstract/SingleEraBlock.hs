{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (
    -- * Single era block
    SingleEraBlock(..)
  , singleEraTransition'
  , proxySingle
    -- * Era index
  , EraIndex(..)
  , eraIndexEmpty
  , eraIndexFromNS
  , eraIndexFromIndex
  , eraIndexZero
  , eraIndexSucc
  , eraIndexToInt
  , KnownEraIndex(..)
  , TypeEqWrapper(..)
  -- , toKnownEraIndex
  , toEraIndex
  ) where

import           Codec.Serialise
import           Data.Either (isRight)
import           Data.Proxy
import           Data.SOP.Strict
import qualified Data.Text as Text
import           Data.Void

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.History (Bound, EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.SOP

import           Cardano.Binary
import           Data.Kind (Type)
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match

{-------------------------------------------------------------------------------
  SingleEraBlock
-------------------------------------------------------------------------------}

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol blk
      , InspectLedger blk
      , LedgerSupportsMempool blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
      , HasPartialConsensusConfig (BlockProtocol blk)
      , HasPartialLedgerConfig blk
      , ConvertRawHash blk
      , ReconstructNestedCtxt Header blk
      , CommonProtocolParams blk
      , LedgerSupportsPeerSelection blk
      , ConfigSupportsNode blk
      , NodeInitStorage blk
      , BlockSupportsMetrics blk
        -- Instances required to support testing
      , Eq   (GenTx blk)
      , Eq   (ApplyTxErr blk)
      , Show blk
      , Show (Header blk)
      , Show (CannotForge blk)
      , Show (ForgeStateInfo blk)
      , Show (ForgeStateUpdateError blk)
      ) => SingleEraBlock blk where

  -- | Era transition
  --
  -- This should only report the transition point once it is stable (rollback
  -- cannot affect it anymore).
  --
  -- Since we need this to construct the 'HardForkSummary' (and hence the
  -- 'EpochInfo'), this takes the /partial/ config, not the full config
  -- (or we'd end up with a catch-22).
  singleEraTransition :: PartialLedgerConfig blk
                      -> EraParams -- ^ Current era parameters
                      -> Bound     -- ^ Start of this era
                      -> LedgerState blk
                      -> Maybe EpochNo

  -- | Era information (for use in error messages)
  singleEraInfo       :: proxy blk -> SingleEraInfo blk

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

singleEraTransition' :: SingleEraBlock blk
                     => WrapPartialLedgerConfig blk
                     -> EraParams
                     -> Bound
                     -> LedgerState blk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . unwrapPartialLedgerConfig

{-------------------------------------------------------------------------------
  Era index
-------------------------------------------------------------------------------}

newtype EraIndex xs = EraIndex {
      getEraIndex :: NS (K ()) xs
    }

instance Eq (EraIndex xs) where
  EraIndex era == EraIndex era' = isRight (matchNS era era')

instance All SingleEraBlock xs => Show (EraIndex xs) where
  show = hcollapse . hcmap proxySingle getEraName . getEraIndex
    where
      getEraName :: forall blk. SingleEraBlock blk
                 => K () blk -> K String blk
      getEraName _ =
            K
          . (\name -> "<EraIndex " <> name <> ">")
          . Text.unpack
          . singleEraName
          $ singleEraInfo (Proxy @blk)

instance All SingleEraBlock xs => Condense (EraIndex xs) where
  condense = hcollapse . hcmap proxySingle getEraName . getEraIndex
    where
      getEraName :: forall blk. SingleEraBlock blk
                 => K () blk -> K String blk
      getEraName _ =
            K
          . Text.unpack
          . singleEraName
          $ singleEraInfo (Proxy @blk)

instance (Typeable xs, SListI xs) => Serialise (EraIndex xs) where
  encode = toCBOR
  decode = fromCBOR

instance (Typeable xs, SListI xs) => ToCBOR (EraIndex xs) where
  toCBOR = encode . nsToIndex . getEraIndex

instance (Typeable xs, SListI xs) => FromCBOR (EraIndex xs) where
  fromCBOR = do
    idx <- decode
    case nsFromIndex idx of
      Nothing       -> fail $ "EraIndex: invalid index " <> show idx
      Just eraIndex -> return (EraIndex eraIndex)

eraIndexEmpty :: EraIndex '[] -> Void
eraIndexEmpty (EraIndex ns) = case ns of {}

eraIndexFromNS :: SListI xs => NS f xs -> EraIndex xs
eraIndexFromNS = EraIndex . hmap (const (K ()))

eraIndexFromIndex :: Index xs blk -> EraIndex xs
eraIndexFromIndex index = EraIndex $ injectNS index (K ())

eraIndexZero :: EraIndex (x ': xs)
eraIndexZero = EraIndex (Z (K ()))

eraIndexSucc :: EraIndex xs -> EraIndex (x ': xs)
eraIndexSucc (EraIndex ix) = EraIndex (S ix)

eraIndexToInt :: EraIndex xs -> Int
eraIndexToInt = index_NS . getEraIndex

-- | x equals to the indexed element in xs
newtype KnownEraIndex (xs :: [Type]) x = KnownEraIndex (NS (TypeEqWrapper x) xs)

newtype TypeEqWrapper x y = TypeEqWrapper (x :~: y)

instance All SingleEraBlock xs => Show (KnownEraIndex xs x) where
  show = show . toEraIndex

toEraIndex :: SListI xs => KnownEraIndex xs x -> EraIndex xs
toEraIndex (KnownEraIndex ns) = EraIndex (hmap (\_ -> (K ())) ns)

-- toKnownEraIndex :: SListI xs => EraIndex xs -> NS (KnownEraIndex xs) xs
-- toKnownEraIndex eraIndex@(EraIndex ns) = let
--   x = hmap (\_ -> K ((), KnownEraIndex _)) ns
--   in _

instance (SListI xs, Typeable xs, Typeable x) => Serialise (KnownEraIndex xs x) where
  encode = toCBOR
  decode = fromCBOR

instance (SListI xs, Typeable xs, Typeable x) => ToCBOR (KnownEraIndex xs x) where
  toCBOR = toCBOR . toEraIndex

instance (SListI xs, Typeable xs, Typeable x) => FromCBOR (KnownEraIndex xs x) where
  fromCBOR = undefined
    -- do
    -- (EraIndex eraIndex) :: EraIndex xs <- fromCBOR
    -- error "TODO fromCBOR (KnownEraIndex xs x)"
    -- -- let checkIndex ns = case ns of
    -- --       Z
    -- -- return $ KnownEraIndex $ EraIndex $ hmap _ eraIndex

instance SameDepIndex (KnownEraIndex xs) where
  sameDepIndex (KnownEraIndex as) (KnownEraIndex bs) = go as bs
    where
      go :: NS (TypeEqWrapper a) xs' -> NS (TypeEqWrapper b) xs' -> Maybe (a :~: b)
      go (Z (TypeEqWrapper Refl)) (Z (TypeEqWrapper Refl)) = Just Refl
      go (S as') (S bs')                                   = go as' bs'
      go _ _                                               = Nothing

