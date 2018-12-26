module Generator where

    import Test.QuickCheck

    data Fool = Fulse | Frue deriving (Eq, Show)

    genFoolEqual :: Gen Fool
    genFoolEqual = elements [Fulse, Frue]

    genFoolBiased :: Gen Fool
    genFoolBiased = frequency [(2, return Fulse), (1, return Frue)]
