module OptionalMonoid where

    import Data.Monoid

    data Optional a = Nada | Only a deriving (Eq, Show)

    instance Monoid (Optional a) where
        mempty = Nada
    instance Semigroup a => Semigroup (Optional a) where
        Nada <> (Only a) = Only a
        (Only a) <> Nada = Only a
        Nada <> Nada = Nada
        (Only a) <> (Only b) = Only (a <> b)