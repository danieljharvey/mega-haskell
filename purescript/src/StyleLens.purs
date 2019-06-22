module StyleLens where

import Prelude
import Data.Array (cons)
import Data.Foldable (foldr)

sample :: String
sample = """
  height: 100px;
  width: 100px;
  background-color: black;
  border: none;
"""

newtype Style p
  = Style (Array (p -> String))

type Props
  = { size :: Int
    , opened :: Boolean
    }

makeStyle :: Style Props
makeStyle =
  Style ( 
    """
    background-color: black;
    "height: 100px;
    """ ||>
    (\p -> "width: " <> (show p.size) <> "px;") $|>
    (\p -> if p.opened 
                 then "border: 1px black solid;" 
                 else "border: none;") $|> 
    []
  )

makeStyle2 :: Style Props
makeStyle2 =
  Style ( 
    _str """
         background-color: black;
         height: 100px;
         """ $
    _fun (\p -> "width: " <> (show p.size) <> "px;") $
    _fun (\p -> if p.opened 
                 then "border: 1px black solid;" 
                 else "border: none;") $ 
    []
  )

makeStyle3 :: Style Props
makeStyle3 =
  Style 
    [ const """
         background-color: black;
         height: 100px;
         """
    , (\p -> "width: " <> (show p.size) <> "px;")
    , (\p -> if p.opened 
                 then "border: 1px black solid;" 
                 else "border: none;")
    ]


constStyle :: forall a. String -> Style a
constStyle s
  = Style (pure (const s))

renderStyle 
  :: forall props
   . props
  -> Style props 
  -> String
renderStyle props (Style styles)
  = foldr (<>) "" $ flap styles props

b :: String
b = renderStyle { size: 10, opened: false } makeStyle

c :: String
c = renderStyle { size: 200, opened: true } makeStyle

makeConst :: forall p s. s -> Array (p -> s) -> Array (p -> s)
makeConst
  = cons <<< const

makeLens :: forall a. a -> Array a -> Array a
makeLens = cons

_str :: forall p s. s -> Array (p -> s) -> Array (p -> s)
_str = makeConst

_fun :: forall a. a -> Array a -> Array a
_fun = makeLens

infixr 5 makeConst as ||>

infixr 5 makeLens as $|>

