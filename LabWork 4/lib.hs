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
   pure a = Singleton a
   (Singleton f) <*> (Singleton a) = Singleton(f a)

instance Monad Singleton where
  -- TODO
  (Singleton a) >>= f = f a

instance Foldable Singleton where
  -- TODO
    foldMap f (Singleton a) = f a
    -- или
    -- foldr = undefined

instance Traversable Singleton where
  -- TODO
   traverse f (Singleton a) = fmap Singleton (f a)
  -- или
  -- traverse = undefined

-- Productish

instance Functor (Productish x) where
  -- TODO
  fmap f (Productish a b) = Productish a (f b) 

instance (Monoid a) => Applicative (Productish a) where
  -- TODO
  pure a = Productish mempty a
  (Productish f g) <*> (Productish a b) = Productish(f <> a) (g b) 

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
-- Either
instance Functor (Summish a) where
  -- TODO
  fmap f (First a) = First a
  fmap f (Second b)=  Second(f b)


instance Applicative (Summish a) where
  -- TODO
  pure b = Second b
  First a <*> f = First a
  (Second b) <*> f = fmap b f

instance Monad (Summish a) where
  -- TODO
  (First a) >>= f = First a 
  (Second a) >>= f = f a

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
-- аналог Maybe
instance Functor Optional where
  -- TODO
  fmap f NoValue = NoValue
  fmap f (HasValue a) = HasValue (f a)

instance Applicative Optional where
  -- TODO
  pure a = HasValue a
  NoValue <*> a = NoValue
  HasValue f <*> a = f <$> a 
  -- (<*>) = undefined

instance Monad Optional where
  -- TODO
  -- (>>=) = undefined
  NoValue >>= f  =  NoValue
  HasValue x  >>= f  =  f x

instance Foldable Optional where
  -- TODO
    foldMap f NoValue = mempty
    foldMap f (HasValue x) = f x
    -- или
    -- foldr = undefined

instance Traversable Optional where
  -- TODO
  traverse f (HasValue a) = fmap HasValue (f a)
  traverse f NoValue = pure (NoValue)
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
  pure a = Value a
  Value f <*> Value x = Value (f x)
  Value f <*> Layer (a) = Layer (f <$> a)
  Layer f <*> a = Layer (f <*> a)
  -- (<*>) = undefined

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
  pure a = LastValue a
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
