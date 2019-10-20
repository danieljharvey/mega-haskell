module StyleLens where

import Prelude
import Data.Foldable (foldr)

sample :: String
sample =
  """
  height: 100px;
  width: 100px;
  background-color: black;
  border: none;
"""

newtype Style p
  = Style (Array (StyleRule p))

instance styleSemigroup :: Semigroup (Style a) where
  append (Style as) (Style bs) = Style (as <> bs)

data StyleRule p
  = Const String
  | Lens (p -> String)

type Props
  = { size :: Int
    , opened :: Boolean
    }

str :: forall props. String -> Style props
str = Style <<< pure <<< Const

fun :: forall props. (props -> String) -> Style props
fun = Style <<< pure <<< Lens

makeStyle :: Style Props
makeStyle =
  str
    """
         background-color: black;
         height: 100px;
         """
    <> fun (\p -> "width: " <> (show p.size) <> "px;")
    <> fun
        ( \p ->
            if p.opened then
              "border: 1px black solid;"
            else
              "border: none;"
        )

renderStyle ::
  forall props.
  props ->
  Style props ->
  String
renderStyle props (Style styles) = foldr (<>) "" $ flap (map everythingRenderer styles) props

makeRenderer ::
  forall props.
  (String -> String) ->
  (String -> String) ->
  StyleRule props ->
  props ->
  String
makeRenderer constF lensF f p = case f of
  Const s -> (constF s)
  Lens a -> lensF (a p)

dynamicRenderer :: forall p. StyleRule p -> p -> String
dynamicRenderer = makeRenderer (const mempty) identity

staticRenderer :: forall p. StyleRule p -> p -> String
staticRenderer = makeRenderer identity (const mempty)

everythingRenderer :: forall p. StyleRule p -> p -> String
everythingRenderer = makeRenderer identity identity

b :: String
b = renderStyle { size: 10, opened: false } makeStyle

c :: String
c = renderStyle { size: 200, opened: true } makeStyle
