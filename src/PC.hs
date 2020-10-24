{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module PC where

------------------------------------------------------------------------------
import           GHC.Exts (Constraint)
------------------------------------------------------------------------------

type Function m i o = i -> m o

type family ListOfConstraintsToConstraint (cs :: [Constraint]) :: Constraint where
  ListOfConstraintsToConstraint        '[]   = ()
  ListOfConstraintsToConstraint   (c ': cs)  = (c, ListOfConstraintsToConstraint cs)

newtype FunctionWithConstraints m cs i o =
  FunctionWithConstraints (ListOfConstraintsToConstraint cs => Function m i o)

type family GetConstraints (k :: ([Constraint],*,*)) where GetConstraints '(a,_,_) = a
type family GetInputs      (k :: ([Constraint],*,*)) where GetInputs      '(_,b,_) = b
type family GetOutput      (k :: ([Constraint],*,*)) where GetOutput      '(_,_,c) = c

type family FunctionsWithConstraints (m :: * -> *) (cios :: [([Constraint],*,*)])
 where
  FunctionsWithConstraints  _          '[]  = ()
  FunctionsWithConstraints  m (cio ': cios) =
    ( FunctionWithConstraints m (GetConstraints cio) (GetInputs cio) (GetOutput cio)
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

type NthConstraints n css = ListOfConstraintsToConstraint (GetConstraints (Lookup n css))

runNthFunction
  :: forall (m :: * -> *) (n :: Nat) (cios :: [([Constraint],*,*)])
   . (Monad m, NthConstraints n cios)
  => Ix             n cios
  -> Functions      m cios
  -> GetInputs    (Lookup n cios)
  -> m (GetOutput (Lookup n cios))
runNthFunction  IZ     (Functions (FunctionWithConstraints f,  _)) i = f i
runNthFunction (IS ix) (Functions (_                        , fs)) i =
  runNthFunction ix (Functions fs) i

------------------------------------------------------------------------------

type family AllConstraints (cios :: [([Constraint],*,*)]) :: Constraint where
  AllConstraints          '[]  = ()
  AllConstraints (cio ': cios) =
    (ListOfConstraintsToConstraint (GetConstraints cio), AllConstraints cios)

data SingL :: [cio] -> * where
  SN ::               SingL      '[]
  SC :: SingL cios -> SingL (cio ': cios)

type family Inputs (cios :: [([Constraint],*,*)]) :: * where
  Inputs          '[]  = ()
  Inputs (cio ': cios) = (GetInputs cio, Inputs cios)

type family Outputs (cios :: [([Constraint],*,*)]) :: * where
  Outputs          '[]  = ()
  Outputs (cio ': cios) = (GetOutput cio, Outputs cios)

runAllFunctions
  :: (Monad m, AllConstraints cios)
  => SingL       cios
  -> Functions m cios
  -> Inputs      cios
  -> m (Outputs  cios)
runAllFunctions  SN                                            _       _  = pure ()
runAllFunctions (SC s) (Functions (FunctionWithConstraints f, fs)) (i,is) = do
  res1 <- f i
  rest <- runAllFunctions s (Functions fs) is
  pure (res1, rest)
