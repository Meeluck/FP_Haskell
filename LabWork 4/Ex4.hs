data PrivatePerson = PrivatePerson {
    personFirstName :: String
  , pesronLastName :: String
  , personBirthYear :: Word
  } deriving (Show)
data Company = Company {
    companyName :: String
  , companyAddress :: String
  }  deriving (Show)

class HasName a where
  name :: a -> String

instance HasName PrivatePerson where
  name p = personFirstName p <> " " <> pesronLastName p

instance HasName Company where
  name = companyName

{-
  они могут быть преобразованы из и в стандартный тип Maybe.
  Объявите класс Maybeish, имеющий функции fromMaybe и toMaybe,
  производящие соответствующие преобразования.
  Напишите экземпляры этого класса для этих типов и Maybe.
-}

data MayHaveValue a = NoValue | HasValue a deriving (Show)
data ToBeOrNot a = NotToBe | ToBe a deriving (Show)
data HasSomething a = HasNothing | HasSomething a deriving (Show)

class Maybeish a where
  fromMaybe :: Maybe b -> a b
  toMaybe :: a b -> Maybe b

instance Maybeish MayHaveValue where
  fromMaybe (Just a) = HasValue a
  fromMaybe Nothing = NoValue
  toMaybe (HasValue a) = Just a
  toMaybe NoValue = Nothing

instance Maybeish ToBeOrNot where
  fromMaybe (Just a) = ToBe a
  fromMaybe Nothing = NotToBe
  toMaybe (ToBe a) = Just a
  toMaybe NotToBe = Nothing

instance Maybeish HasSomething where
  fromMaybe (Just a) = HasSomething a
  fromMaybe Nothing = HasNothing
  toMaybe (HasSomething a) = Just a
  toMaybe HasNothing = Nothing

instance Maybeish Maybe where
  fromMaybe a = a
  toMaybe a = a

