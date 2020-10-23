{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ExampleV where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text              as T
import           GHC.Exts               (Constraint)
------------------------------------------------------------------------------

-- see test/ExampleVSpec.hs for usage

------------------------------------------------------------------------------

type Function m i o = i -> m o

type family ListOfConstraintsToConstraint (cs :: [Constraint]) :: Constraint where
  ListOfConstraintsToConstraint        '[]   = ()
  ListOfConstraintsToConstraint   (c ': cs)  = (c, ListOfConstraintsToConstraint cs)

newtype FunctionWithConstraints m cs i o =
  FunctionWithConstraints (ListOfConstraintsToConstraint cs => Function m i o)

type family Fst (k :: (a,b,c)) where Fst '(a,b,c) = a
type family Snd (k :: (a,b,c)) where Snd '(a,b,c) = b
type family Thr (k :: (a,b,c)) where Thr '(a,b,c) = c

type family FunctionsWithConstraints (m :: * -> *) (cios :: [([Constraint],*,*)])
 where
  FunctionsWithConstraints  _          '[]  = ()
  FunctionsWithConstraints  m (cio ': cios) =
    ( FunctionWithConstraints m (Fst cio) (Snd cio) (Thr cio)
    , FunctionsWithConstraints m cios )

data Functions (m :: * -> *) (cios :: [([Constraint],*,*)]) where
  Functions :: FunctionsWithConstraints m cios -> Functions m cios

------------------------------------------------------------------------------

data Nat = Z | S Nat

data Ix :: Nat -> [cio] -> * where
  IZ ::              Ix  'Z    (cio ': cios)
  IS :: Ix n cios -> Ix ('S n) (cio ': cios)

type family Lookup (n :: Nat) (xs :: [k]) :: k where
  Lookup  'Z    (k ':  _) = k
  Lookup ('S n) (_ ': ks) = Lookup n ks

type NthConstraint n css = ListOfConstraintsToConstraint (Fst (Lookup n css))

runNthFunction
  :: forall (m :: * -> *) (n :: Nat) (cios :: [([Constraint],*,*)])
   . (Monad m, NthConstraint n cios)
  => Ix             n cios
  -> Functions      m cios
  -> Snd    (Lookup n cios)
  -> m (Thr (Lookup n cios))
runNthFunction  IZ     (Functions (FunctionWithConstraints f,  _)) i = f i
runNthFunction (IS ix) (Functions (_                        , fs)) i =
  runNthFunction ix (Functions fs) i

------------------------------------------------------------------------------

type family AllConstraintsFrom (cios :: [([Constraint],*,*)]) :: Constraint where
  AllConstraintsFrom          '[]  = ()
  AllConstraintsFrom (cio ': cios) =
    (ListOfConstraintsToConstraint (Fst cio), AllConstraintsFrom cios)

data SingL :: [cio] -> * where
  SN ::               SingL      '[]
  SC :: SingL cios -> SingL (cio ': cios)

type family Inputs (cios :: [([Constraint],*,*)]) :: * where
  Inputs          '[]  = ()
  Inputs (cio ': cios) = (Snd cio, Inputs cios)

type family Outputs (cios :: [([Constraint],*,*)]) :: * where
  Outputs          '[]  = ()
  Outputs (cio ': cios) = (Thr cio, Outputs cios)

runAllFunctions
  :: (Monad m, AllConstraintsFrom cios)
  => SingL       cios
  -> Functions m cios
  -> Inputs      cios
  -> m (Outputs  cios)
runAllFunctions  SN                                            _       _  = pure ()
runAllFunctions (SC s) (Functions (FunctionWithConstraints f, fs)) (i,is) = do
  res1 <- f i
  rest <- runAllFunctions s (Functions fs) is
  pure (res1, rest)

-- ============================================================================

f1 :: MonadIO m => Function m Text Int
f1 t = do
  liftIO (print t)
  pure (read (T.unpack t))

f2 :: (MonadPlus m, Num a, Ord a, Show a) => Function m (a,a) Text
f2 (n1, n2) = do
  guard (n1 > n2)
  pure (T.pack (show (n1 + n2)))

twoFs :: Functions (m :: * -> *)
                   '[ '( '[ MonadIO m ]                        ,  Text,  Int)
                    , '( '[ MonadPlus m, Num a, Ord a, Show a ], (a,a) , Text)
                    ]
twoFs  = Functions (FunctionWithConstraints f1, (FunctionWithConstraints f2, ()))
