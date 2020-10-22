{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module ExampleV where

------------------------------------------------------------------------------
import           Control.Monad.Except
import           Data.Text            as T
import           GHC.Exts             (Constraint)
------------------------------------------------------------------------------

-- see test/ExampleVSpec.hs for usage

------------------------------------------------------------------------------

type Function i m a = i -> m a

type family ListOfConstraintsToConstraint (cs :: [Constraint]) :: Constraint where
  ListOfConstraintsToConstraint        '[]   = ()
  ListOfConstraintsToConstraint   (c ': cs)  = (c, ListOfConstraintsToConstraint cs)

newtype FunctionWithConstraints cs i m a =
  FunctionWithConstraints (ListOfConstraintsToConstraint cs => Function i m a)

type family FunctionsWithConstraints (css :: [[Constraint]]) (i :: *) (m :: * -> *) (a :: *)
 where
  FunctionsWithConstraints        '[]  i m a = ()
  FunctionsWithConstraints (cs ': css) i m a =
    (FunctionWithConstraints cs i m a, FunctionsWithConstraints css i m a)

data Functions (css :: [[Constraint]]) (i :: *) (m :: * -> *) (a :: *) where
  Functions :: FunctionsWithConstraints css i m a -> Functions css i m a

------------------------------------------------------------------------------

data Nat = Z | S Nat

data Ix :: Nat -> [k] -> * where
  IZ ::            Ix  'Z    (x ': xs)
  IS :: Ix n xs -> Ix ('S n) (x ': xs)

type family Lookup (n :: Nat) (xs :: [k]) :: k where
  Lookup  'Z    (x ': _xs) = x
  Lookup ('S n) (x ':  xs) = Lookup n xs

type NthConstraint ix css = ListOfConstraintsToConstraint (Lookup ix css)

runNthFunction
  :: (Monad m, NthConstraint n css)
  => Ix n css
  -> Functions css i m a
  -> i
  -> m a
runNthFunction  IZ     (Functions (FunctionWithConstraints f,  _)) i = f i
runNthFunction (IS ix) (Functions (_                        , fs)) a =
  runNthFunction ix (Functions fs) a

------------------------------------------------------------------------------

type family ListOfListOfConstraintsToConstraint (cs :: [[Constraint]]) :: Constraint where
  ListOfListOfConstraintsToConstraint        '[]  = ()
  ListOfListOfConstraintsToConstraint (cs ': css) =
    (ListOfConstraintsToConstraint cs, ListOfListOfConstraintsToConstraint css)

{- For parameterized err to work, need to index the return value.
   In that case, might as well fully generalize return value (not just Either).
data SingL :: [k] -> * where
  SN ::             SingL      '[]
  SC :: SingL xs -> SingL (x ': xs)

type family NTupleOf (css :: [k]) (err :: *) (a :: *) :: * where
  NTupleOf      '[]  err a = ()
  NTupleOf (_ ': xs) err a = (Either err a, NTupleOf xs err a)

runAllFunctions :: (Monad m, Allcc css) => SingL css -> Functions css err m a -> a -> m (NTupleOf css err a)
runAllFunctions  SN                  _   _ = return ()
runAllFunctions (SC s) (Functions (FunctionWithConstraints f, fs)) a = do
  res1 <- runExceptT (f a)
  rest <- runAllFunctions s (Functions fs) a
  return (res1, rest)
-}
-- ============================================================================

myapp1 :: MonadIO m => Function Int m Text
myapp1 i = do
  liftIO (print i)
  pure (T.pack (show i))

myapp2 :: MonadPlus m => Function Int m Text
myapp2 i = do
  guard (i > 0)
  pure (T.pack (show i))

twoFs :: Functions '[ '[MonadIO m], '[MonadPlus m] ] Int m Text
twoFs  = Functions (FunctionWithConstraints myapp1, (FunctionWithConstraints myapp2, ()))

