module Show where

data Thing = Thing1 | Thing2

-- showing these items will break...
-- show Thing1

data BetterThing = Better1 | Better2

instance Show BetterThing where
    show Better1 = "Better thing 1"
    show Better2 = "The other even better thing"

showBetter1 :: String
showBetter1 = show Better1
-- showBetter1 = "Better thing 1"

data LazyThing = Lazy | Crazy | Other deriving (Show)

showLazy :: String
showLazy = show Lazy
-- showLazy == "Lazy"


type Href = String

data Tree a = Div [Tree a] 
            | Title String 
            | P String 
            | A Href [Tree a]


website :: Tree String
website = Div [ Title "My interesting website"
           , P "Item of interest"
           , A "http://danieljharvey.github.io" [ P "Website" ]
           , Div [ P "Sub-item"
                 , P "Another sub-item"
                 ]
           ]

-- utility function for showing a list of tree items
-- and combining the output
concatSubtags :: (Show a) => [Tree a] -> String
concatSubtags as = concat $ fmap show as

-- show instance for our Tree type
instance (Show a) => Show (Tree a) where
    show (Div as) = "<div>" ++ concatSubtags as ++ "</div>"
    show (A href as) = "<a href='" ++ href ++ "'>" 
                     ++ concatSubtags as ++ "</a>"
    show (P a) = "<p>" ++ a ++ "</p>"
    show (Title a) = "<h1>" ++ a ++ "</h1>"
    

