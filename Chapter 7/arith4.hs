-- read :: Read a => String -> a
-- show :: Show a => a -> String

-- roundTrip :: (Show a, Read a) => a -> a
-- roundTrip = read . show

-- main = do
--     print (roundTrip 4)
--     print (id 4)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show
    
main = do
    print ((roundTrip 4) :: Int)
    print (id 4)