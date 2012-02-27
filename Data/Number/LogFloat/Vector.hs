{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Data.Number.LogFloat.Vector () where

import Data.Number.LogFloat
import Data.Vector.Unboxed
import Control.Monad
import qualified Data.Vector.Unboxed.Base
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
  
newtype instance MVector s LogFloat = MV_LogFloat (MVector s Double)
newtype instance Vector    LogFloat = V_LogFloat  (Vector    Double)
instance Unbox LogFloat

instance M.MVector MVector LogFloat where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_LogFloat v) = M.basicLength v
  basicUnsafeSlice i n (MV_LogFloat v) = MV_LogFloat $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_LogFloat v1) (MV_LogFloat v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_LogFloat `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n rec = MV_LogFloat `liftM` M.basicUnsafeReplicate n (logFromLogFloat rec)
  basicUnsafeRead (MV_LogFloat v) i = logToLogFloat `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_LogFloat v) i rec = M.basicUnsafeWrite v i $ logFromLogFloat rec
  basicClear (MV_LogFloat v) = M.basicClear v
  basicSet (MV_LogFloat v) rec = M.basicSet v $ logFromLogFloat rec
  basicUnsafeCopy (MV_LogFloat v1) (MV_LogFloat v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_LogFloat v1) (MV_LogFloat v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_LogFloat v) n = MV_LogFloat `liftM` M.basicUnsafeGrow v n

instance G.Vector Vector LogFloat where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_LogFloat v) = V_LogFloat `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_LogFloat v) = MV_LogFloat `liftM` G.basicUnsafeThaw v
  basicLength (V_LogFloat v) = G.basicLength v
  basicUnsafeSlice i n (V_LogFloat v) = V_LogFloat $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_LogFloat v) i = logToLogFloat `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_LogFloat mv) (V_LogFloat v) = G.basicUnsafeCopy mv v
  elemseq _ rec y = G.elemseq (undefined :: Vector Double) (logFromLogFloat rec) y

