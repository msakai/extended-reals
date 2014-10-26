{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ExtendedReal
-- Copyright   :  (c) Masahiro Sakai 2014
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (DeriveDataTypeable)
--
-- Extension of real numbers with positive/negative infinities (±∞).
-- It is useful for describing various limiting behaviors in mathematics.
--
-- Remarks:
--
-- * @∞ - ∞@ is left undefined as usual,
--   but we define @0 × ∞ = 0 × -∞ = 0@ by following the convention of
--   probability or measure theory.
--
-- References:
--
-- * Wikipedia contributors, "Extended real number line," Wikipedia,
--   The Free Encyclopedia, https://en.wikipedia.org/wiki/Extended_real_number_line
--   (accessed September 1, 2014).
--
-----------------------------------------------------------------------------
module Data.ExtendedReal
  ( Extended (..)
  , inf
  , isFinite
  , isInfinite
  ) where

import Prelude hiding (isInfinite)
import Control.DeepSeq
import Data.Data
import Data.Hashable
import Data.Typeable

-- | @Extended r@ is an extension of /r/ with positive/negative infinity (±∞).
data Extended r
  = NegInf    -- ^ negative infinity (-∞)
  | Finite !r -- ^ finite value
  | PosInf    -- ^ positive infinity (+∞)
  deriving (Ord, Eq, Show, Read, Typeable, Data)

instance Bounded (Extended r) where
  minBound = NegInf
  maxBound = PosInf

instance Functor Extended where
  fmap _ NegInf = NegInf
  fmap f (Finite x) = Finite (f x)
  fmap _ PosInf = PosInf

instance NFData r => NFData (Extended r) where
  rnf (Finite x) = rnf x
  rnf _ = ()

instance Hashable r => Hashable (Extended r) where
  hashWithSalt s NegInf     = s `hashWithSalt` (0::Int)
  hashWithSalt s (Finite x) = s `hashWithSalt` (1::Int) `hashWithSalt` x
  hashWithSalt s PosInf     = s `hashWithSalt` (2::Int)

-- | Infinity (∞)
inf :: Extended r
inf = PosInf

-- | @isFinite x = not (isInfinite x)@.
isFinite :: Extended r -> Bool
isFinite (Finite _) = True
isFinite _ = False

-- | @isInfinite x@ returns @True@ iff @x@ is @PosInf@ or @NegInf@.
isInfinite :: Extended r -> Bool
isInfinite = not . isFinite

-- | Note that @Extended r@ is /not/ a field, nor a ring.
-- 
-- @PosInf + NegInf@ is left undefined as usual,
-- but we define @0 * PosInf = 0 * NegInf = 0@ by following the convention of probability or measure theory.
instance (Num r, Ord r) => Num (Extended r) where
  Finite a + Finite b = Finite (a+b)
  PosInf + NegInf = error "PosInf + NegInf is undefined"
  NegInf + PosInf = error "NegInf + PosInf is undefined"
  PosInf + _ = PosInf
  _ + PosInf = PosInf
  NegInf + _ = NegInf
  _ + NegInf = NegInf

  Finite x1 * e = scale x1 e
  e * Finite x2 = scale x2 e
  PosInf * PosInf = PosInf
  PosInf * NegInf = NegInf
  NegInf * PosInf = NegInf
  NegInf * NegInf = PosInf

  negate NegInf = PosInf
  negate (Finite x) = Finite (negate x)
  negate PosInf = NegInf

  abs NegInf = PosInf
  abs (Finite x) = Finite (abs x)
  abs PosInf = PosInf

  signum NegInf = Finite (-1)
  signum (Finite x) = Finite (signum x)
  signum PosInf = Finite 1

  fromInteger = Finite . fromInteger  

-- | Note that @Extended r@ is /not/ a field, nor a ring.
instance (Fractional r, Ord r) => Fractional (Extended r) where
  recip (Finite x) = Finite (1/x)
  recip _ = Finite 0

  fromRational = Finite . fromRational

-- Note that we define @0 * PosInf = 0 * NegInf = 0@ by the convention of probability or measure theory.
scale :: (Num r, Ord r) => r -> Extended r -> Extended r
scale a e = seq e $
  case a `compare` 0 of
    EQ -> Finite 0
    GT ->
      case e of
        NegInf   -> NegInf
        Finite b -> Finite (a*b)
        PosInf   -> PosInf
    LT ->
      case e of
        NegInf   -> PosInf
        Finite b -> Finite (a*b)
        PosInf   -> NegInf
