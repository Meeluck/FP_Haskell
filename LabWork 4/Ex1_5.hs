import Data.Function -- здесь определена ф-я on
import Control.Applicative -- здесь определена ф-я liftA2

instance Eq PersonWithAge where
  (==) = (liftA2 . liftA2) (&&)
      ((==) `on` personName)
      ((==) `on` personAge)
