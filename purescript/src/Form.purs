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
  Form (internal -> Form' internal a)

type Form' internal a
  = { ui :: (internal -> Effect Unit) -> UI
    , result :: Maybe a
    }

derive instance formFunctor :: Functor (Form internal)

instance applyForm :: Apply (Form internal) where
  apply (Form f) (Form x) = Form \internal ->
     let { ui: uiF, result: resultF } = f internal
         { ui: uiX, result: resultX } = x internal
     in { ui: uiF <> uiX 
        , result: resultF <*> resultX
        }

instance applicativeForm :: Applicative (Form internal) where
  pure a = Form \_ ->
    { ui: mempty
    , result: pure a
    }

textForm :: Form String NES.NonEmptyString
textForm 
  = makeForm (\_ -> "<input type='text' />")
             NES.fromString

ageForm :: Form Int Natural
ageForm 
  = makeForm (\_ -> "<input type='number' />")
             (Just <<< intToNat)

makeForm :: forall i a. ((i -> Effect Unit) -> UI) -> (i -> Maybe a) -> Form i a
makeForm render validate
  = Form (\i -> { ui: render, result: validate i })

focus :: forall i j a. Lens' i j -> Form j a -> Form i a
focus lens (Form f) =
  Form \newVal ->
    let
      { ui, result } = f (view lens newVal)
    in
      { ui: \onChange -> ui (onChange <<< flip (set lens) newVal)
      , result
      }

type SampleData
  = { name :: String
    , age  :: Int
    }

type ValidatedData
  = { name :: NES.NonEmptyString
    , age  :: Natural
    }

bigForm :: Form SampleData ValidatedData
bigForm = ado
  name <- focus (prop (SProxy :: SProxy "name")) textForm
  age  <- focus (prop (SProxy :: SProxy "age")) ageForm
     in { name, age }

getFormUI 
  :: forall i a
   . Form i a 
  -> (i -> Effect Unit) 
  -> i 
  -> UI
getFormUI (Form form) update i
   = (form i).ui update

getFormResult 
  :: forall i a
   . Form i a 
  -> i 
  -> Maybe a
getFormResult (Form form) i
  = (form i).result

sampleData :: SampleData
sampleData = { name: "Eggs"
             , age: 100
             }

form1 :: UI
form1 = getFormUI bigForm (\_ -> pure unit) sampleData

form1Result :: Maybe ValidatedData 
form1Result = getFormResult bigForm sampleData


