{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-| The UTXO state, kept in memory by the chain index.
-}
module Plutus.ChainIndex.UtxoState(
    UtxoState(..)
    , usTxUtxoData
    , usTip
    , UtxoIndex
    , utxoState
    , tip
    -- * Extending the UTXO index
    , InsertUtxoPosition(..)
    , InsertUtxoSuccess(..)
    , InsertUtxoFailed(..)
    , insert
    -- * Rollbacks
    , RollbackFailed(..)
    , RollbackResult(..)
    , viewTip
    ) where

import           Control.Lens                      (makeLenses, view)
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.FingerTree                   (FingerTree, Measured (..))
import qualified Data.FingerTree                   as FT
import           Data.Function                     (on)
import           Data.Semigroup.Generic            (GenericSemigroupMonoid (..))
import           GHC.Generics                      (Generic)
import           Plutus.ChainIndex.ChainIndexError (InsertUtxoFailed (..), RollbackFailed (..))
import           Plutus.ChainIndex.ChainIndexLog   (InsertUtxoPosition (..))
import           Plutus.ChainIndex.Types           (Tip (..))
import           Prettyprinter                     (Pretty (..))

-- | UTXO / ledger state, kept in memory. We are only interested in the UTXO set, everything else is stored
--   on disk. This is OK because we don't need to validate transactions when they come in.
data UtxoState a =
    UtxoState
        { _usTxUtxoData :: a -- One of 'TxUtxoBalance', 'TxOutBalance' or 'TxIdState'
        , _usTip        :: Tip -- ^ Tip of our chain sync client
        }
        deriving stock (Eq, Show, Generic)
        deriving (Semigroup, Monoid) via (GenericSemigroupMonoid (UtxoState a))
        deriving anyclass (FromJSON, ToJSON)

makeLenses ''UtxoState

type UtxoIndex a = FingerTree (UtxoState a) (UtxoState a)
instance Monoid a => Measured (UtxoState a) (UtxoState a) where
    measure = id

utxoState :: Measured (UtxoState a) (UtxoState a)
          => UtxoIndex a
          -> UtxoState a
utxoState = measure

tip :: UtxoState a -> Tip
tip = view usTip

instance Eq a => Ord (UtxoState a) where
    compare = compare `on` tip

data InsertUtxoSuccess a =
    InsertUtxoSuccess
        { newIndex       :: UtxoIndex a
        , insertPosition :: InsertUtxoPosition
        }

instance Pretty (InsertUtxoSuccess a) where
  pretty = \case
    InsertUtxoSuccess _ insertPosition -> pretty insertPosition

-- | Insert a 'UtxoState' into the index
insert ::
       ( Measured (UtxoState a) (UtxoState a)
       , Eq a
       )
       => UtxoState a
       -> FingerTree (UtxoState a) (UtxoState a)
       -> Either InsertUtxoFailed (InsertUtxoSuccess a)
insert   UtxoState{_usTip=TipAtGenesis} _ = Left InsertUtxoNoTip
insert s@UtxoState{_usTip=thisTip} ix =
    let (before, after) = FT.split (s <=)  ix
    in case tip (measure after) of
        TipAtGenesis -> Right $ InsertUtxoSuccess{newIndex = before FT.|> s, insertPosition = InsertAtEnd}
        t | t > thisTip -> Right $ InsertUtxoSuccess{newIndex = (before FT.|> s) <> after, insertPosition = InsertBeforeEnd}
          | otherwise   -> Left  $ DuplicateBlock t

data RollbackResult a =
    RollbackResult
        { newTip          :: Tip
        , rolledBackIndex :: UtxoIndex a
        }

viewTip :: Measured (UtxoState a) (UtxoState a)
        => UtxoIndex a
        -> Tip
viewTip = tip . measure
