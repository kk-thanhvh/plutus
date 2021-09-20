{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
module Plutus.ChainIndex.TxUtxoBalance where

import           Control.Lens                (view)
import           Data.FingerTree             (Measured (measure))
import qualified Data.FingerTree             as FT
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Ledger                      (TxIn (txInRef), TxOutRef (..))
import           Plutus.ChainIndex.Tx        (ChainIndexTx (..), citxInputs, txOutsWithRef)
import           Plutus.ChainIndex.Types     (Point (..), Tip (..), TxUtxoBalance (..), pointsToTip, tubUnspentOutputs)
import           Plutus.ChainIndex.UtxoState (RollbackFailed (OldPointNotFound, RollbackNoTip, TipMismatch, foundTip, targetPoint),
                                              RollbackResult (RollbackResult, newTip, rolledBackIndex), UtxoIndex,
                                              UtxoState (UtxoState, _usTip, _usTxUtxoData), tip, usTxUtxoData, viewTip)

fromTx :: ChainIndexTx -> TxUtxoBalance
fromTx tx =
    TxUtxoBalance
        { _tubUnspentOutputs = Set.fromList $ fmap snd $ txOutsWithRef tx
        , _tubUnmatchedSpentInputs = Set.mapMonotonic txInRef (view citxInputs tx)
        }

-- | Whether a 'TxOutRef' is a member of the UTXO set (ie. unspent)
isUnspentOutput :: TxOutRef -> UtxoState TxUtxoBalance -> Bool
isUnspentOutput r = Set.member r . view (usTxUtxoData . tubUnspentOutputs)

-- | The UTXO set
unspentOutputs :: UtxoState TxUtxoBalance -> Set TxOutRef
unspentOutputs = view (usTxUtxoData . tubUnspentOutputs)

-- | 'UtxoIndex' for a single block
fromBlock :: Tip -> [ChainIndexTx] -> UtxoState TxUtxoBalance
fromBlock tip_ transactions =
    UtxoState
            { _usTxUtxoData = foldMap fromTx transactions
            , _usTip        = tip_
            }

-- | Perform a rollback on the utxo index
rollback :: Point
         -> UtxoIndex TxUtxoBalance
         -> Either RollbackFailed (RollbackResult TxUtxoBalance)
rollback _ (viewTip -> TipAtGenesis) = Left RollbackNoTip
rollback targetPoint idx@(viewTip -> currentTip)
    -- The rollback happened sometime after the current tip.
    | not (targetPoint `pointLessThanTip` currentTip) =
        Left TipMismatch{foundTip=currentTip, targetPoint}
    | otherwise = do
        let (before, _) = FT.split (pointLessThanTip targetPoint . tip) idx

        case tip (measure before) of
            TipAtGenesis -> Left $ OldPointNotFound targetPoint
            oldTip | targetPoint `pointsToTip` oldTip ->
                       Right RollbackResult{newTip=oldTip, rolledBackIndex=before}
                   | otherwise                        ->
                       Left  TipMismatch{foundTip=oldTip, targetPoint=targetPoint}
    where
      pointLessThanTip :: Point -> Tip -> Bool
      pointLessThanTip PointAtGenesis  _               = True
      pointLessThanTip (Point pSlot _) (Tip tSlot _ _) = pSlot < tSlot
      pointLessThanTip _               TipAtGenesis    = False

