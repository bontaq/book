{-# LANGUAGE InstanceSigs #-}

module Ch25 where

import Control.Monad

newtype MaybeIO a =
  MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a =
  MaybeList { runMaybeList :: [Maybe a] }

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) =
    IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity ab) <*> (Identity a) = Identity $ ab a

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f =
    let
      aimb = join $ fmap runIdentityT (fmap f ma)
      -- fmap (f . g) == fmap f . fmap g
      aimb' = join $ fmap (runIdentityT . f) ma
      aimb'' = ma >>= (runIdentityT . f)
    in IdentityT aimb'

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }
