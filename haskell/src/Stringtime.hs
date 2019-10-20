{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Stringtime where

import Data.Hashable
import Data.Kind
import Data.Map.Lazy
import Data.Monoid ((<>))
import Data.Text (Text, concat, intercalate, pack)
import Prelude hiding (concat, foldr)

data Stuff
  = Stuff
      { stName :: Text,
        stAge :: Integer
      }

type Template = Stuff -> Text

parts :: [Template]
parts =
  [ const "<html><head>",
    \s -> "<title>" <> stName s <> "</title>",
    const "</head>",
    const "<body>",
    \s -> "<h1>Let's have a nice time with the number " <> pack (show (stAge s)) <> "!!!</h1>",
    const "</body></html>"
  ]

render :: Stuff -> [Template] -> Text
render s ts =
  concat $ fmap apply ts
  where
    apply t = t s

-- HList of props

data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

instance Show (HList '[]) where
  show _ = ""

instance
  (Show a, Show (HList as)) =>
  Show (HList (a ': as))
  where
  show (HCons x HNil) =
    show x
  show (HCons x xs) =
    show x ++ ":" ++ show xs

instance Eq (HList '[]) where
  HNil == HNil = True

instance
  (Eq a, Eq (HList as)) =>
  Eq (HList (a ': as))
  where
  (HCons x xs) == (HCons y ys) =
    x == y && xs == ys

instance Ord (HList '[]) where
  HNil <= HNil = True

instance
  (Ord a, Ord (HList as)) =>
  Ord (HList (a ': as))
  where
  (HCons x xs) <= (HCons y ys) =
    x <= y

--

class CartesianStyles (as :: [Type]) where
  cartesian :: [HList as]

instance CartesianStyles '[] where
  cartesian = []

instance
  {-# OVERLAPPING #-}
  (Bounded a, Enum a) =>
  CartesianStyles '[a]
  where
  cartesian = fmap hCons [minBound .. maxBound]
    where
      hCons b = HCons b HNil

instance
  (CartesianStyles as, Bounded a, Enum a) =>
  CartesianStyles (a ': as)
  where
  cartesian = do
    x <- [minBound .. maxBound]
    xs <- cartesian
    pure (HCons x xs)

--

type CSS = [Text]

type Classname = Text

renderCSS :: CSS -> Text
renderCSS css =
  "." <> cls <> " { " <> intercalate "; " css <> "; }"
  where
    cls = hashCSS css

hashCSS :: CSS -> Classname
hashCSS css = pack $ "cart" ++ show (hash (concat css))

type StyleMap props = Map props Text

propsToClass :: (a -> CSS) -> a -> Classname
propsToClass render a = hashCSS (render a)

createStylesheet ::
  (Ord (HList as), CartesianStyles as) =>
  (HList as -> CSS) ->
  [Text]
createStylesheet render =
  elems $ fromList pairs
  where
    pairs =
      fmap (\p -> (className p, output p)) cartesian
    className p =
      hashCSS (render p)
    output p =
      renderCSS (render p)

-- concrete types for one component

data SwitchState = On | Off
  deriving (Enum, Bounded, Show, Eq, Ord)

data FocusState = NotFocused | Focused | Blurred
  deriving (Enum, Bounded, Show, Eq, Ord)

type TitleStyle = HList '[SwitchState, FocusState]

createTitleStyles :: TitleStyle -> CSS
createTitleStyles (HCons switch (HCons focus _)) =
  ["background-color: " <> (if switch == On then "green" else "red")]
    <> ["border: " <> (if focus == Focused then "10px black solid" else "0")]

getComponentClass :: TitleStyle -> Classname
getComponentClass = propsToClass createTitleStyles
{-
data Dict (c :: Constraint) where
  Dict :: c => Dict c

f :: Dict (Eq Int)
f = Dict

isEqual :: (Typeable a, Typeable b) => a -> b -> Maybe (Dict (a ~ b))
isEqual = eqTypeRep

data Renderable where
  Make :: (Typeable a, Renderable a) => a -> Renderable

diff :: Renderable -> Renderable -> Patch
diff x y = case eqTypeRep x y of
  Nothing -> [ delete x, add y]
  Just Refl -> calculatePatch x y

calculatePatch :: a -> a -> ...

data x :~: y where
    Refl :: x :~: x

data Title
  = Title
      { switch :: SwitchState
      , focus :: FocusState
      }
  deriving Generic -- !!!!!!!!

class TypeToHList (x :: Type) (xs :: [Type]) where
  convert :: x -> HList xs

generate :: (Generic x, TypeToList x xs, CartesianProductino xs) => [x]
y
-}
