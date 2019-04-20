{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Free where

import           Control.Monad.Free
import qualified Control.Monad.State       as St
import           Control.Monad.Writer.Lazy hiding (Sum)
import           Data.Functor.Sum
import           Data.Kind
import           Network.Curl
import           Prelude
import           System.Exit

-- our first data type

data ConsoleF a
  = Write String a
  | Read (String -> a)

instance Functor ConsoleF where
    fmap f (Write s next) = Write s (f next)
    fmap f (Read next)    = Read (f . next)

type Console a = Free ConsoleF a

fWrite :: String -> Console ()
fWrite str = liftF $ Write str ()

fRead :: Console String
fRead = liftF $ Read id

fStop :: Console ()
fStop = pure ()

consoleProg :: Console ()
consoleProg = do
    fWrite "What is your name?"
    a <- fRead
    fWrite $ "Sure? " ++ a
    b <- fRead
    fWrite "Great."

interpretIO :: Console a -> IO a
interpretIO
  = foldFree interpret
  where
    interpret :: ConsoleF a -> IO a
    interpret prog'
      = case prog' of
          Write s a -> Prelude.putStrLn s >> pure a
          Read a    -> a <$> Prelude.getLine

interpretWrite :: Console a -> Writer [String] a
interpretWrite = foldFree interpretConsoleWrite

interpretConsoleWrite :: ConsoleF a -> Writer [String] a
interpretConsoleWrite prog'
  = case prog' of
      Write s a -> do
        _ <- tell [s]
        pure a
      Read a -> do
        _ <- tell ["wait for input"]
        pure (a "input")

-- and another

data ReducerF s a
  = Modify (s -> s) a
  | Get (s -> a)
  | Fetch String (String -> a)
  deriving (Functor)

type Reducer s a
  = Free (ReducerF s) a

data State
  = State { string  :: Maybe String
          , url     :: String
          , loading :: Bool
          }
  deriving (Eq, Ord, Show)

modify :: (s -> s) -> Reducer s ()
modify f = liftF $ Modify f ()

fetch :: String -> Reducer s String
fetch url = liftF $ Fetch url id

get :: Reducer s s
get = liftF $ Get id

fetchAction :: Reducer State ()
fetchAction = do
  modify (\s
    -> s { loading = True })
  state <- get
  str <- fetch (url state)
  modify (\s
    -> s { loading = False, string = Just str })

interpretState :: Reducer State a -> St.State State a
interpretState = foldFree interpretReducerState

interpretReducerState :: ReducerF State a -> St.State State a
interpretReducerState prog'
  = case prog' of
      Modify f next -> do
        St.modify f
        pure next
      Get next ->
        next <$> St.get
      Fetch url next ->
        pure (next "test item")

interpretStateIO :: Reducer State a -> St.StateT State IO a
interpretStateIO = foldFree interpretReducerStateIO

interpretReducerStateIO :: ReducerF State a -> St.StateT State IO a
interpretReducerStateIO prog'
  = case prog' of
      Modify f next -> do
        St.modify f
        pure next
      Get next ->
        next <$> St.get
      Fetch url next -> do
        (_, s) <- lift $ curlGetString url []
        pure (next s)

-- snd <$> runStateT (interpretStateIO fetchAction) initialState

initialState :: State
initialState
  = State { string = Nothing
          , url = "http://internetisverymuchmybusiness.com"
          , loading = False
          }

-- and combine them!


type CombinedF s = Sum (ConsoleF) (ReducerF s)

type Combined s a
  = Free (CombinedF s) a

write' :: String -> Combined s ()
write' str
  = liftF $ InL $ Write str ()

read' :: Combined s String
read'
  = liftF $ InL $ Read id

modify' :: (s -> s) -> Combined s ()
modify' f
  = liftF $ InR $ Modify f ()

fetch' :: String -> Combined s String
fetch' url
  = liftF $ InR $ Fetch url id

get' :: Combined s s
get'
  = liftF $ InR $ Get id

loadAndLog :: Combined State ()
loadAndLog = do
  modify' (\s
    -> s { loading = True })
  state <- get'
  str <- fetch' (url state)
  write' str -- let's log that string out
  modify' (\s
    -> s { loading = False, string = Just str })

-- let's make an interpreter from ConsoleF to any MonadIO
-- this will work with any Monad stack that includes IO

interpretConsoleStateIO
  :: (MonadIO m)
  => ConsoleF a
  -> m a
interpretConsoleStateIO prog'
  = case prog' of
      Write s a -> liftIO $ Prelude.putStrLn s >> pure a
      Read a    -> liftIO $ a <$> Prelude.getLine

interpretCombinedStateIO
  :: Combined State a
  -> St.StateT State IO a
interpretCombinedStateIO = foldFree interpret'
  where
    interpret' side
      = case side of
          InL a -> interpretConsoleStateIO a
          InR a -> interpretReducerStateIO a

-- Infix version of Sum functor
type f :+: g = Sum f g

-- our new functor, using both things
type LumpF s = ReducerF s :+: ConsoleF

type Lump s a = Free (LumpF s) a

class Lifty'
  (big :: Type -> Type)
  (small :: Type -> Type)
  (directions :: Maybe [Direction])
  | big directions -> small where
  lifty' :: small a -> big a

data (x :: k) :~: (y :: k) where
  Refl :: x :~: x

-- example using this new Lifty thing
loadAndLogF
  :: (Functor (big State), Lifty (big State) ConsoleF, Lifty (big State) (ReducerF State))
  => Free (big State) ()
loadAndLogF = do
  modifyF (\s
    -> s { loading = True })
  state <- getF
  str <- fetchF (url state)
  writeF str -- let's log that string out
  modifyF (\s
    -> s { loading = False, string = Just str })


-- now we can write our constructors for these new free using constraints
writeF
  :: (Functor big, Lifty big ConsoleF)
  => String
  -> Free big ()
writeF str
  = liftF $ lifty $ Write str ()

readF
  :: (Functor big, Lifty big ConsoleF)
  => Free big String
readF
  = liftF $ lifty $ Read id

modifyF
  :: (Functor (big s), Lifty (big s) (ReducerF s))
  => (s -> s)
  -> Free (big s) ()
modifyF f
  = liftF $ lifty $ Modify f ()

fetchF
  :: forall big s. (Functor (big s), Lifty (big s) (ReducerF s))
  => String
  -> Free (big s) String
fetchF url
  = liftF $ lifty $ (Fetch @s) url id

getF
  :: (Functor (big s), Lifty (big s) (ReducerF s))
  => Free (big s) s
getF
  = liftF $ lifty $ Get id










test0 :: Breadcrums (LumpF s) ConsoleF :~: 'Just '[R]
test0 = Refl

test1 :: String -> Lump s ()
test1 s = liftF $ lifty (Write s ())

class Lifty (big :: Type -> Type) (small :: Type -> Type) where
  lifty :: small a -> big a

type family Breadcrums (big :: Type -> Type) (small :: Type -> Type) :: Maybe [Direction] where
  Breadcrums a a = 'Just '[]
  Breadcrums (Sum x y) a = MapDirection L (Breadcrums x a)
                       <|> MapDirection R (Breadcrums y a)
  Breadcrums _ _ = 'Nothing

type family (x :: Maybe k) <|> (y :: Maybe k) :: Maybe k where
  'Just a <|> _ = 'Just a
  a      <|> b = b

type family MapDirection (direction :: Direction) (directions :: Maybe [Direction]) :: Maybe [Direction] where
  MapDirection a ('Just directions) = 'Just (a ': directions)
  MapDirection _ ('Nothing)         = 'Nothing

data Direction = L | R

instance (big ~ small) => Lifty' big small ('Just '[]) where
  lifty' a = a

instance (Lifty' l small ('Just xs))
  => Lifty' (Sum l r) small ('Just (L ': xs)) where
    lifty' a = InL (lifty' @_ @_ @(Just xs) a)

instance (Lifty' r small ('Just xs))
  => Lifty' (Sum l r) small ('Just (R ': xs)) where
    lifty' a = InR (lifty' @_ @_ @(Just xs) a)

instance (Lifty' big small (Breadcrums big small)) => Lifty big small where
  lifty = lifty' @_ @_ @(Breadcrums big small)
