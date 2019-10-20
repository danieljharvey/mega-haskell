module State where

import Control.Monad.State
import Data.List
import Data.List.Index
import Data.Maybe (isJust)
import Data.Semigroup
import Prelude

type Href = String

data Style = Color String | BackgroundColor String | Width Int | Height Int deriving (Show, Eq)

data StyleTree
  = Div [Style] [StyleTree]
  | Title [Style] String
  | P [Style] String
  | A [Style] Href [StyleTree]

website :: StyleTree
website =
  Div
    [Width 100, Height 200]
    [ Title [Color "red"] "My interesting website",
      P [Color "red"] "Item of interest",
      A [] "http://danieljharvey.github.io" [P [] "Website"],
      Div
        [BackgroundColor "#FFDD00"]
        [ P [] "Sub-item",
          P [] "Another sub-item"
        ]
    ]

renderStyle :: Style -> String
renderStyle (Color s) = "color: " ++ s
renderStyle (BackgroundColor s) = "background-color: " ++ s
renderStyle (Width w) = "width: " ++ show w ++ "px"
renderStyle (Height h) = "height: " ++ show h ++ "px"

addSemi :: String -> String
addSemi s = s ++ ";"

renderStyles :: [Style] -> String
renderStyles [] = ""
renderStyles as = concat ["style=\"", intercalate ", " $ map renderStyle as, "\""]

getStyleNumber :: [[Style]] -> [Style] -> (Maybe Int, [[Style]])
getStyleNumber styles [] = (Nothing, styles)
getStyleNumber [] style = (Just 0, [style])
getStyleNumber styles style = (index, allStyles)
  where
    index = elemIndex style allStyles
    allStyles =
      if isJust $ elemIndex style styles
        then styles
        else styles <> [style]

getStateStyleClass :: [Style] -> State [[Style]] (Maybe Int)
getStateStyleClass style = do
  styles <- get
  let (index, newStyles) = getStyleNumber styles style
  put newStyles
  pure index

renderStyleClass :: String -> String
renderStyleClass str = concat ["class=\"", str, "\""]

className :: Int -> String
className i = "genClass" ++ show i

getClassTag :: Maybe Int -> String
getClassTag = maybe "" (renderStyleClass . className)

renderCSS :: Int -> [Style] -> String
renderCSS _ [] = ""
renderCSS i as =
  concat
    [ ".",
      className i,
      " {\n",
      intercalate "\n" $ map (addSemi . renderStyle) as,
      "\n}"
    ]

renderAllCSS :: [[Style]] -> String
renderAllCSS as = concat $ fmap (uncurry renderCSS) $ indexed as

stateStyleTree :: StyleTree -> State [[Style]] String
stateStyleTree (Div style children) = do
  index <- getStateStyleClass style
  rendered <- mapM stateStyleTree children
  pure $
    concat
      [ "<div ",
        getClassTag index,
        ">",
        concat rendered,
        "</div>"
      ]
stateStyleTree (Title style text) = do
  index <- getStateStyleClass style
  pure $
    concat
      [ "<h1 ",
        getClassTag index,
        ">",
        text,
        "</h1>"
      ]
stateStyleTree (P style text) = do
  index <- getStateStyleClass style
  pure $
    concat
      [ "<p ",
        getClassTag index,
        ">",
        text,
        "</p>"
      ]
stateStyleTree (A style href children) = do
  index <- getStateStyleClass style
  rendered <- mapM stateStyleTree children
  pure $
    concat
      [ "<a href=\"",
        href,
        "\" ",
        getClassTag index,
        ">",
        concat rendered,
        "</div>"
      ]

renderStyleTree :: String
renderStyleTree = html ++ "/n/n" ++ "<style>" ++ renderAllCSS css ++ "</style>"
  where
    (html, css) = runState (stateStyleTree website) []
