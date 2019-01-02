import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    Nada <> (Only a) = Only a
    (Only a) <> Nada = Only a
    (Only a) <> (Only b) = Only (a <> b)
    Nada <> Nada = Nada

-- I think the superclass for Monoid was updated since the book was written, and any instance of Monoid
-- also requires a dummny instance of Semigroup. As such, I didn't really understand this solution, and
-- came across this at https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in
