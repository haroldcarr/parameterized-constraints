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
import           GHC.Exts             (Constraint)
------------------------------------------------------------------------------

-- see test/ExampleVSpec.hs for usage

------------------------------------------------------------------------------

type ApplyFn1 m a = a -> m a

type family Allc (cs :: [Constraint]) :: Constraint where
  Allc      '[]     = ()
  Allc (c ': cs)    = (c, Allc cs)

type family Allcc (cs :: [[Constraint]]) :: Constraint where
  Allcc        '[]  = ()
  Allcc (cs ': css) = (Allc cs, Allcc css)

newtype App1 cs m a = App1 (Allc cs => ApplyFn1 m a)

type family AppZs (css :: [[ Constraint]]) m a where
  AppZs        '[]  m a = ()
  AppZs (cs ': css) m a = (App1 cs m a, AppZs css m a)

data ApplyZ (css :: [ [ Constraint ] ]) (m :: * -> *) (a :: *) where
  AZ :: AppZs css m a -> ApplyZ css m a

------------------------------------------------------------------------------

data Ix :: [k] -> * where
  Here  ::          Ix (x ': xs)
  There :: Ix xs -> Ix (x ': xs)

doIx :: (Monad m, Allcc css) => Ix css -> ApplyZ css m a -> a -> m a
doIx Here       (AZ (App1 f, _)) a = f a
doIx (There ix) (AZ (_,     fs)) a = doIx ix (AZ fs) a

data Nat = Z | S Nat

data Ix' :: Nat -> [k] -> * where
  Here'  ::             Ix'  'Z    (x ': xs)
  There' :: Ix' n xs -> Ix' ('S n) (x ': xs)

type family Lookup (n :: Nat) (xs :: [k]) :: k where
  Lookup  'Z    (x ': _xs) = x
  Lookup ('S n) (x ':  xs) = Lookup n xs

type AllIx ix css = Allc (Lookup ix css)

doIx' :: (Monad m, AllIx n css) => Ix' n css -> ApplyZ css m a -> a -> m a
doIx' Here'       (AZ (App1 f, _)) a = f a
doIx' (There' ix) (AZ (_,     fs)) a = doIx' ix (AZ fs) a

------------------------------------------------------------------------------

data SingL :: [k] -> * where
  SN ::             SingL      '[]
  SC :: SingL xs -> SingL (x ': xs)

type family Tupfor (css :: [k]) (a :: *) :: * where
  Tupfor      '[]  a = ()
  Tupfor (_ ': xs) a = (a, Tupfor xs a)

doAll :: (Monad m, Allcc css) => SingL css -> ApplyZ css m a -> a -> m (Tupfor css a)
doAll SN                    _   _ = return ()
doAll (SC s) (AZ (App1 f, fs)) a = do
  res1 <- f a
  rest <- doAll s (AZ fs) a
  return (res1, rest)

-- ============================================================================

myapp1 :: MonadIO m => ApplyFn1 m Int
myapp1 i = do
  liftIO (print i)
  pure i

myapp2 :: MonadPlus m => ApplyFn1 m Int
myapp2 i = do
  guard (i > 0)
  pure i

twoFs :: ApplyZ '[ '[MonadIO m], '[MonadPlus m] ] m Int
twoFs = AZ (App1 myapp1, (App1 myapp2, ()))

