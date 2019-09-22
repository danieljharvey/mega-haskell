module Form where

import Prelude
import Effect (Effect)
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..)) 
import Data.Natural (Natural, intToNat)
import Data.Symbol (SProxy(..))
import Data.String.NonEmpty as NES

type UI = String

newtype Form internal a =
  Form (internal -> { ui :: (internal -> Effect Unit) -> UI, result :: Maybe a })

derive instance formFunctor :: Functor (Form internal)

instance applyFormBuilder :: Apply (Form internal) where
  apply (Form f) (Form x) = Form \internal ->
     let { ui: uiF, result: resultF } = f internal
         { ui: uiX, result: resultX } = x internal
     in { ui: uiF <> uiX 
        , result: resultF <*> resultX
        }

instance applicativeFormBuilder :: Applicative (Form internal) where
  pure a = Form \_ ->
    { ui: mempty
    , result: pure a
    }

textForm :: Form String NES.NonEmptyString
textForm = Form (\text -> { ui: \_ -> "<input type='text' />"
                          , result: NES.fromString text
                          })

ageForm :: Form Int Natural
ageForm = Form (\int -> { ui: \_ -> "<input type='number' />"
                        , result: Just (intToNat int)
                        })


focus :: forall i j a. Lens' i j -> Form j a -> Form i a
focus l (Form f) =
  Form \i ->
    let
      { ui, result } = f (view l i)
    in
      { ui: \onChange -> ui (onChange <<< flip (set l) i)
      , result
      }

type SampleData
  = { name :: String
    , age  :: Int
    }

bigForm :: Form SampleData String
bigForm = ado
  name <- focus (prop (SProxy :: SProxy "name")) textForm
  age  <- focus (prop (SProxy :: SProxy "age")) ageForm
  in "My name is " <> show name <> " and I am " <> show age


