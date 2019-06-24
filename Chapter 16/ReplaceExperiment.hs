module ReplaceExperiment where

    replaceWithP :: b -> Char
    replaceWithP = const 'p'

    lms :: [Maybe [Char]]
    lms = [Just "Ave", Nothing, Just "woohoo"]
    
    -- Do not lift at all
    replaceWithP' :: [Maybe [Char]] -> Char
    replaceWithP' = replaceWithP
    
    -- Lift once
    liftedOnce :: Functor f => f a -> f Char
    liftedOnce = fmap replaceWithP

    -- Lift twice
    liftedTwice :: (Functor f, Functor f1) => f (f1 a) -> f (f1 Char)
    liftedTwice = (fmap . fmap) replaceWithP

    -- Lift thrice
    liftedThrice :: (Functor f, Functor f1, Functor f2) => f (f1 (f2 a)) -> f (f1 (f2 Char))
    liftedThrice = (fmap . fmap . fmap) replaceWithP