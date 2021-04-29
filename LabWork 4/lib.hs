module Lib where

data Singleton a = Singleton a deriving (Eq, Show)
data Productish a b = Productish a b deriving (Eq, Show)
data Summish a b = First a | Second b deriving (Eq, Show)
data Optional a = NoValue | HasValue a deriving (Eq, Show)
data NotQuiteList a = Value a | Layer (NotQuiteList a) deriving (Eq, Show)
data NotEmpty a = LastValue a | MidValue a (NotEmpty a) deriving (Eq, Show)

-- Singleton
-- fmap :: (a -> b) -> f a -> f b

instance Functor Singleton where
  -- TODO
  fmap f (Singleton x) = Singleton (f x) 

-- (<*>) :: f (a -> b) -> f a -> f b
instance Applicative Singleton  where
  -- TODO
   pure = undefined
  (<*>) = undefined

instance Monad Singleton where
  -- TODO
  (>>=) = undefined

instance Foldable Singleton where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable Singleton where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined

-- Productish

instance Functor (Productish x) where
  -- TODO
  fmap f (Productish a b) = Productish a (f b) 

instance (Monoid a) => Applicative (Productish a) where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance (Monoid a) => Monad (Productish a) where
  -- TODO
  (>>=) = undefined

instance Foldable (Productish a) where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable (Productish a) where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined

-- Summish


-- data Summish a b = First a | Second b deriving (Eq, Show)

instance Functor (Summish a) where
  -- TODO
  fmap f (First a) = First a
  fmap f (Second b)=  Second(f b)


instance Applicative (Summish a) where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad (Summish a) where
  -- TODO
  (>>=) = undefined

instance Foldable (Summish a) where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable (Summish a) where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined


-- Optional
-- data Optional a = NoValue | HasValue a deriving (Eq, Show)

instance Functor Optional where
  -- TODO
  fmap f NoValue = NoValue
  fmap f (HasValue a) = HasValue (f a)

instance Applicative Optional where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad Optional where
  -- TODO
  (>>=) = undefined

instance Foldable Optional where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable Optional where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined


-- NotQuiteList
-- data NotQuiteList a = Value a | Layer (NotQuiteList a) deriving (Eq, Show)

instance Functor NotQuiteList where
  -- TODO
  fmap f (Value a) = Value (f a)
  fmap f (Layer a) = Layer (fmap f a)

instance Applicative NotQuiteList where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad NotQuiteList where
  -- TODO
  (>>=) = undefined

instance Foldable NotQuiteList where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable NotQuiteList where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined

-- NotEmpty
-- data NotEmpty a = LastValue a | MidValue a (NotEmpty a) deriving (Eq, Show)

instance Functor NotEmpty where
  -- TODO
  fmap f (LastValue a) = LastValue (f a)
  fmap f (MidValue a b) = MidValue(f a) (fmap f b)

instance Applicative NotEmpty where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad NotEmpty where
  -- TODO
  (>>=) = undefined

instance Foldable NotEmpty where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable NotEmpty where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined
