{-# LANGUAGE DeriveFunctor #-}

module Form where

import           Data.Semigroup

data UI action
  = Empty
  | Container [UI action]
  | Item String action
  deriving (Show, Eq, Functor)

instance Semigroup (UI a) where
  Empty        <> b            = b
  a            <> Empty        = a
  Container as <> Item s b     = Container (as <> [Item s b])
  Item s a     <> Container bs = Container ([Item s a] <> bs)
  a            <> b            = Container [a, b]

instance Monoid (UI a) where
  mappend = (<>)
  mempty = Empty

data Form state action
  = Form { render   :: state -> UI action
         , process  :: action -> state -> state
         , validate :: state -> Bool
         }

newtype CounterState
  = CounterState { weight :: Int }

data CounterAction
  = Increase
  | Decrease
  deriving (Show)

counterProcess :: CounterAction -> CounterState -> CounterState
counterProcess Increase st = st { weight = weight st + 1 }
counterProcess Decrease st = st { weight = weight st - 1 }

counterValidate :: CounterState -> Bool
counterValidate st
  = weight st > 40 && weight st < 100

counterRender :: CounterState -> UI CounterAction
counterRender st
  = Container [ Item "down" Decrease, Item "up" Increase ]

form :: Form CounterState CounterAction
form = Form { render = counterRender
            , process = counterProcess
            , validate = counterValidate
            }

testCol = Container [ Item "down" Decrease, Item "up" Increase ]

testEmpty = Empty

testButton = Item "Bum" Decrease




type UI2 = String

newtype Form2 i a =
  Form2 { getForm2 :: i -> FormReturn2 i a }

instance Functor (Form2 i) where
  fmap f form =
    Form2 (\i -> f <$> ((getForm2 form) i))

data FormReturn2 i a
  = FormReturn2 { render2 :: (i -> IO ()) -> UI2
                , result2 :: Maybe a
                }

instance Functor (FormReturn2 i) where
  fmap f (FormReturn2 render2 result2 )
    = FormReturn2 { render2 = render2, result2 =  (f <$> result2) }

