module EffectAndAff where

import Prelude (Unit, bind, discard, pure, unit, ($), mempty)
import Data.Show (class Show, show)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer
import Effect.Console (log)
import Effect.Aff (Aff, Canceler, Error, makeAff, runAff_)

newtype TimeReturn
  = TimeReturn TimeoutId

instance showTimeoutId :: Show TimeReturn where
  show _ = "TIMEOUTID"

callbackWithDelay :: forall a. a -> (a -> Effect Unit) -> Effect TimeReturn
callbackWithDelay a cb = do
  timeoutId <- (setTimeout 100 (cb a))
  pure (TimeReturn timeoutId)

manyCallbacks :: forall a. a -> (a -> Effect Unit) -> Effect Unit
manyCallbacks a cb = do
  log "1"
  _ <-
    callbackWithDelay a
      ( \b -> do
          log "2"
          _ <-
            callbackWithDelay b
              ( \c -> do
                  log "3"
                  _ <-
                    callbackWithDelay c
                      ( \d -> do
                          log "3"
                          _ <-
                            callbackWithDelay d
                              ( \e -> do
                                  log "4"
                                  _ <-
                                    callbackWithDelay e
                                      ( \f -> do
                                          log "5"
                                          _ <-
                                            callbackWithDelay f
                                              ( \g -> do
                                                  log "6"
                                                  _ <-
                                                    callbackWithDelay g
                                                      ( \h -> do
                                                          log "7"
                                                          _ <-
                                                            callbackWithDelay h
                                                              ( \i -> do
                                                                  log "8"
                                                                  _ <-
                                                                    callbackWithDelay i
                                                                      ( \j -> do
                                                                          log "9"
                                                                          _ <- cb j
                                                                          pure unit
                                                                      )
                                                                  pure unit
                                                              )
                                                          pure unit
                                                      )
                                                  pure unit
                                              )
                                          pure unit
                                      )
                                  pure unit
                              )
                          pure unit
                      )
                  pure unit
              )
          pure unit
      )
  pure unit

delayAff :: forall a. a -> Aff a
delayAff a = makeAff affCallback
  where
  affCallback :: (Either Error a -> Effect Unit) -> Effect Canceler
  affCallback success = do
    _ <- callbackWithDelay a (\b -> success (Right b))
    pure mempty

  makeCanceller :: TimeoutId -> Error -> Aff Unit
  makeCanceller timer =
    ( \_ -> do
        liftEffect $ clearTimeout timer
        pure unit
    )

goTimer :: forall a. (Show a) => a -> Effect Unit
goTimer a = do
  runAff_ (\answer -> log (show answer)) (affTimer a)

affTimer :: forall a. a -> Aff a
affTimer a = do
  b <- delayAff a
  liftEffect $ log "1"
  c <- delayAff b
  liftEffect $ log "2"
  d <- delayAff c
  liftEffect $ log "3"
  e <- delayAff d
  liftEffect $ log "4"
  f <- delayAff e
  liftEffect $ log "5"
  g <- delayAff f
  liftEffect $ log "6"
  h <- delayAff g
  liftEffect $ log "7"
  i <- delayAff h
  liftEffect $ log "8"
  j <- delayAff i
  liftEffect $ log "9"
  delayAff j
