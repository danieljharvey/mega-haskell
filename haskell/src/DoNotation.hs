module DoNotation where

import           Prelude

main :: IO ()
main = do
    firstName <- getLine
    surname <- getLine
    print ("Hello " ++ firstName ++ " " ++ surname)

main2 :: IO ()
main2 = getLine
    >>= (\firstName -> getLine
    >>= (\surname ->
        print ("Hello " ++ firstName ++ " " ++ surname)))

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

safeHeadTwice :: [[a]] -> Maybe a
safeHeadTwice aas = do
    as <- safeHead aas
    safeHead as

safeHeadTwice2 :: [[a]] -> Maybe a
safeHeadTwice2 aas = safeHead aas
            >>= (\as -> safeHead as
            >>= (\a -> return a))

safeHeadTwiceShort :: [[a]] -> Maybe a
safeHeadTwiceShort a = safeHead a >>= safeHead
