module Datatype where

    import Test.QuickCheck

    data Fool = Fulse | Frue deriving (Eq, Show)

    -- Equal probabilities
    genEqual :: Gen Fool
    genEqual = elements [Fulse, Frue]

    instance Arbitrary Fool where
        arbitrary = genEqual

    -- Different probabilities
    genUnequal :: Gen Fool
    genUnequal = frequency [(2, return Fulse), (1, return Frue)]