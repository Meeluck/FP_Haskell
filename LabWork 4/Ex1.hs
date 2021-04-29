data PersonWithAgeAndAddress
   = PersonWithAgeAndAddress {
        pwaName :: String
      , pwaAge :: Word
      , pwaAddress :: String
      } deriving (Eq)

instance Ord PersonWithAgeAndAddress where
  compare = (compare `on` pwaName)
          ? (compare `on` pwaAge)
          ? (compare `on` pwaAddress)
    where
    infixr 6 ? -- так же, как и у <>
    (f1 ? f2) x y = f1 x y <> f2 x y
    -- здесь примерно эквивалентно
    -- (?) = (liftA2 . liftA2) (<>)

-- Проверим типы


    :t (?)
    (?):: Semigroup a =>(t1 -> t2 -> a) -> (t1 -> t2 -> a) -> t1 -> t2 -> a
    1) первый параметр бинарная операция
    2) второй параметр бинарная операция
    3) тип возвращаемого значения бинарная операция
    тип возвращаемого значения совпадает с типоми агрумента, ошибки не будет


    (f1 ? f2) x y = compare (f1 x) (f1 y) <> compare (f2 x) (f2 y)
    :t (?)
    (?):: (Ord a1, Ord a2) => (t -> a1) -> (t -> a2) -> t -> t -> Ordering
    1) первый параметр унарная операция
    2) второй параметр унарная операция
    3) тип возвращаемого значения бинарная операция
    Возникнет ошибка, т.к. тип возвращаемого значения не совпадает с типоми агрумента

--не выполняется ассоциативность
