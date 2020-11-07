{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module PC2 where

------------------------------------------------------------------------------
import           GHC.Exts (Constraint)
------------------------------------------------------------------------------

type Function m i o = i -> m o

type family ListOfConstraintsToConstraint (cs :: [Constraint]) :: Constraint where
  ListOfConstraintsToConstraint        '[]   = ()
  ListOfConstraintsToConstraint   (c ': cs)  = (c, ListOfConstraintsToConstraint cs)

newtype FunctionWithConstraints m cs i o =
  FunctionWithConstraints (ListOfConstraintsToConstraint cs => Function m i o)

type family FunctionsWithConstraints (m :: * -> *) (css :: [[Constraint]]) (iss :: [*]) (os :: [*])
 where
  FunctionsWithConstraints  _        '[]           _         _  = ()
  FunctionsWithConstraints  m (cs ': css) (is ': iss) (o ': os) =
    ( FunctionWithConstraints  m cs  is  o
    , FunctionsWithConstraints m css iss os )

data Functions (m :: * -> *) (css :: [[Constraint]]) (iss :: [*]) (os :: [*]) where
  Functions :: FunctionsWithConstraints m cs iss os -> Functions m cs iss os

------------------------------------------------------------------------------

data Nat = Z | S Nat

data Ix :: Nat -> [css] -> [is] -> [o] -> * where
  IZ ::                    Ix  'Z    (cs ': css) (is ': iss) (o ': os)
  IS :: Ix n css iss os -> Ix ('S n) (cs ': css) (is ': iss) (o ': os)

type family Lookup (n :: Nat) (xs :: [k]) :: k where
  Lookup  'Z    (k ':  _) = k
  Lookup ('S n) (_ ': ks) = Lookup n ks

type NthConstraints n css = ListOfConstraintsToConstraint (Lookup n css)

runNthFunction
  :: (Monad m, NthConstraints n css)
  => Ix        n css iss os
  -> Functions m css iss os
  -> Lookup    n     iss
  -> m (Lookup n os)
runNthFunction  IZ     (Functions (FunctionWithConstraints f,  _)) i =
  f i
runNthFunction (IS ix) (Functions (_                        , fs)) i =
  runNthFunction ix (Functions fs) i

type family AllConstraints (css :: [[Constraint]]) :: Constraint where
  AllConstraints        '[]  = ()
  AllConstraints (cs ': css) = (ListOfConstraintsToConstraint cs, AllConstraints css)
