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
import Data.Validation.Semigroup (V(..), toEither)

type UI = String

newtype Form internal e a =
  Form (internal -> Form' internal e a)

type Form' internal e a
  = { ui :: (internal -> Effect Unit) -> UI
    , result :: V e a
    }

-------------------

derive instance formFunctor :: Functor (Form internal e)

instance applyForm :: Semigroup e => Apply (Form internal e) where
  apply (Form f) (Form x) = Form \internal ->
     let { ui: uiF, result: resultF } = f internal
         { ui: uiX, result: resultX } = x internal
     in  { ui: uiF <> uiX 
         , result: resultF <*> resultX
         }

instance applicativeForm :: Semigroup e => Applicative (Form internal e) where
  pure a = Form \_ ->
    { ui: mempty
    , result: pure a
    }

-------------------

makeForm :: forall i e a. ((i -> Effect Unit) -> UI) -> (i -> Either e a) -> Form i e a
makeForm render validate
  = Form (\i -> { ui: render, result: V (validate i) })

focus :: forall i j e a. Lens' i j -> Form j e a -> Form i e a
focus lens (Form f) =
  Form \newVal ->
    let
      { ui, result } = f (view lens newVal)
    in
      { ui: \onChange -> ui (onChange <<< flip (set lens) newVal)
      , result
      }

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
  = toEither $ (form i).result

------------------------------------

data FormError
  = TextIsEmpty
  | InvalidNat Int

instance showFormError :: Show FormError where
  show TextIsEmpty    = "TextIsEmpty"
  show (InvalidNat i) = "InvalidNat: " <> (show i)

type FormErrors = Array FormError

validateString :: String -> Either FormErrors NES.NonEmptyString
validateString s
  = case NES.fromString s of
      Just a -> Right a
      _      -> Left (pure TextIsEmpty)

validateNat :: Int -> Either FormErrors Natural
validateNat i 
  = if i < 0
    then Left (pure (InvalidNat i))
    else Right (intToNat i)    

textForm :: Form String FormErrors NES.NonEmptyString
textForm 
  = makeForm (\_ -> "<input type='text' />")
             (validateString)

ageForm :: Form Int FormErrors Natural
ageForm 
  = makeForm (\_ -> "<input type='number' />")
             (validateNat)

type SampleData
  = { name :: String
    , age  :: Int
    }

type ValidatedData
  = { nameValid :: NES.NonEmptyString
    , ageValid  :: Natural
    }

bigForm :: Form SampleData FormErrors ValidatedData
bigForm = ado
  name <- focus (prop (SProxy :: SProxy "name")) textForm
  age  <- focus (prop (SProxy :: SProxy "age")) ageForm
     in { nameValid: name
        , ageValid: age
        }

sampleData :: SampleData
sampleData = { name: "Eggs"
             , age: 100
             }

brokenData :: SampleData
brokenData = { name: ""
             , age: -100
             }

form1 :: UI
form1 = getFormUI bigForm (\_ -> pure unit) sampleData

form1Result :: Either FormErrors ValidatedData 
form1Result = getFormResult bigForm sampleData

form2Result :: Either FormErrors ValidatedData
form2Result = getFormResult bigForm brokenData
