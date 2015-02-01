{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Prelude hiding (isInfinite)
import Control.DeepSeq
import Control.Exception (SomeException, evaluate, try)
import Control.Monad
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Data.ExtendedReal

-- ----------------------------------------------------------------------

instance Arbitrary r => Arbitrary (Extended r) where
  arbitrary = 
    oneof
    [ return NegInf
    , return PosInf
    , liftM Finite arbitrary
    ]

eval :: a -> Maybe a
eval a = unsafePerformIO $ do
  ret <- try (evaluate a)
  case ret of
    Left (_::SomeException) -> return Nothing
    Right b -> return $ Just b

isDefined :: a -> Bool
isDefined = isJust . eval

-- ----------------------------------------------------------------------

prop_add_comm :: Property
prop_add_comm =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
    eval (a + b) == eval (b + a)

prop_add_assoc :: Property
prop_add_assoc =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    eval (a + (b + c)) == eval ((a + b) + c)

prop_add_unit :: Property
prop_add_unit =
  forAll arbitrary $ \(a :: Extended Rational) ->
    0 + a == a

prop_add_monotone :: Property
prop_add_monotone =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    a <= b && isDefined (a+c) && isDefined (b+c)
    ==> a+c <= b+c

prop_mult_comm :: Property
prop_mult_comm =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
    a * b == b * a

-- PosInf + NegInf is left undefined
case_add_PosInf_NegInf :: IO ()
case_add_PosInf_NegInf =
  eval (inf + (- inf) :: Extended Rational) @?= Nothing

prop_mult_assoc :: Property
prop_mult_assoc =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    a * (b * c) == (a * b) * c

prop_mult_unit :: Property
prop_mult_unit =
  forAll arbitrary $ \(a :: Extended Rational) ->
    1 * a == a

prop_mult_dist :: Property
prop_mult_dist =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    isDefined (a * (b + c)) && isDefined (a * b + a * c)
    ==> eval (a * (b + c)) == eval (a * b + a * c)

prop_mult_zero :: Property
prop_mult_zero = 
  forAll arbitrary $ \(a :: Extended Rational) ->
    0 * a == 0

prop_mult_monotone :: Property
prop_mult_monotone =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    a <= b && c > 0 && isDefined (a*c) && isDefined (b*c)
    ==> a*c <= b*c

-- We define 0 * PosInf = 0
case_mult_zero_PosInf :: IO ()
case_mult_zero_PosInf =
  0 * inf @?= (0 :: Extended Rational)

-- We define 0 * NegInf = 0
case_mult_zero_NegInf :: IO ()
case_mult_zero_NegInf =
  0 * (- inf) @?= (0 :: Extended Rational)

prop_negate_inverse :: Property
prop_negate_inverse = 
  forAll arbitrary $ \(a :: Extended Rational) ->
    negate (negate a) == a

prop_signum_abs :: Property
prop_signum_abs =
  forAll arbitrary $ \(a :: Extended Rational) ->
    signum a * abs a == a

prop_recip_inverse :: Property
prop_recip_inverse =
  forAll arbitrary $ \(a :: Extended Rational) ->
    isFinite a && a /= 0 ==> recip (recip a) == a

case_recip_PosInf :: IO ()
case_recip_PosInf = recip inf @?= (0 :: Extended Rational)

case_recip_NegInf :: IO ()
case_recip_NegInf = recip (- inf) @?= (0 :: Extended Rational)

prop_minBound_smallest :: Property
prop_minBound_smallest =
  forAll arbitrary $ \(a :: Extended Rational) ->
    minBound <= a

prop_maxBound_largest :: Property
prop_maxBound_largest =
  forAll arbitrary $ \(a :: Extended Rational) ->
    a <= maxBound

prop_isFinite_fromRational :: Property
prop_isFinite_fromRational =
  forAll arbitrary $ \a -> isFinite (fromRational a :: Extended Rational)

prop_isInfinite_PosInf :: Property
prop_isInfinite_PosInf = property $ isInfinite PosInf

prop_isInfinite_NegInf :: Property
prop_isInfinite_NegInf = property $ isInfinite NegInf

-- ----------------------------------------------------------------------
-- Functor

prop_Functor_id :: Property
prop_Functor_id =
  forAll arbitrary $ \(a :: Extended Integer) ->
    fmap id a == a

prop_Functor_comp :: Property
prop_Functor_comp =
  forAll arbitrary $ \(f :: Fun Integer Integer) ->
  forAll arbitrary $ \(g :: Fun Integer Integer) ->
  forAll arbitrary $ \(a :: Extended Integer) ->
    fmap (apply f . apply g) a == fmap (apply f) (fmap (apply g) a)

-- ----------------------------------------------------------------------
-- Show / Read

prop_read_show :: Property
prop_read_show =
  forAll arbitrary $ \(a :: Extended Rational) ->
    read (show a) == a

-- ----------------------------------------------------------------------
-- deepseq

prop_deepseq :: Property
prop_deepseq =
  forAll arbitrary $ \(a :: Extended Rational) ->
    a `deepseq` () == ()

-- ----------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
