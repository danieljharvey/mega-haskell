module Form where

import Prelude
import Effect (Effect)
import Data.Either (Either(..))
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..)) 
import Data.Natural (Natural, intToNat)
import Data.Symbol (SProxy(..))
import Data.String.NonEmpty as NES

type UI = String

newtype Form internal e a =
  Form (internal -> Form' internal e a)

type Form' internal e a
  = { ui :: (internal -> Effect Unit) -> UI
    , result :: Either e a
    }

derive instance formFunctor :: Functor (Form internal e)

instance applyForm :: Apply (Form internal e) where
  apply (Form f) (Form x) = Form \internal ->
     let { ui: uiF, result: resultF } = f internal
         { ui: uiX, result: resultX } = x internal
     in { ui: uiF <> uiX 
        , result: resultF <*> resultX
        }

instance applicativeForm :: Applicative (Form internal e) where
  pure a = Form \_ ->
    { ui: mempty
    , result: pure a
    }

data FormError
  = TextIsEmpty
  | InvalidNat

type FormErrors = Array FormError

validateString :: String -> Either FormErrors NES.NonEmptyString
validateString s
  = case NES.fromString s of
      Just a -> Right a
      _      -> Left (pure TextIsEmpty)

textForm :: Form String FormErrors NES.NonEmptyString
textForm 
  = makeForm (\_ -> "<input type='text' />")
             validateString

ageForm :: Form Int FormErrors Natural
ageForm 
  = makeForm (\_ -> "<input type='number' />")
             (Right <<< intToNat)

makeForm :: forall i e a. ((i -> Effect Unit) -> UI) -> (i -> Either e a) -> Form i e a
makeForm render validate
  = Form (\i -> { ui: render, result: validate i })

focus :: forall i j e a. Lens' i j -> Form j e a -> Form i e a
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

bigForm :: Form SampleData FormErrors ValidatedData
bigForm = ado
  name <- focus (prop (SProxy :: SProxy "name")) textForm
  age  <- focus (prop (SProxy :: SProxy "age")) ageForm
     in { name, age }

getFormUI 
  :: forall i e a
   . Form i e a 
  -> (i -> Effect Unit) 
  -> i 
  -> UI
getFormUI (Form form) update i
   = (form i).ui update

getFormResult 
  :: forall i e a
   . Form i e a 
  -> i 
  -> Either e a
getFormResult (Form form) i
  = (form i).result

sampleData :: SampleData
sampleData = { name: "Eggs"
             , age: 100
             }

form1 :: UI
form1 = getFormUI bigForm (\_ -> pure unit) sampleData

form1Result :: Either FormErrors ValidatedData 
form1Result = getFormResult bigForm sampleData


