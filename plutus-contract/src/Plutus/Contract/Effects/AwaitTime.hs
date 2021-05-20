{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Plutus.Contract.Effects.AwaitTime where

import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Row
import           Data.Text.Prettyprint.Doc.Extras
import           GHC.Generics                     (Generic)
import           Prelude                          hiding (until)

import           Plutus.Contract.Request          as Req
import           Plutus.Contract.Schema           (Event (..), Handlers (..), Input, Output)
import           Plutus.Contract.Types            (AsContractError, Contract, selectEither)
import           Plutus.Contract.Util             (foldMaybe)

import           Ledger.Time                      (POSIXTime (..))

type TimeSymbol = "time"

type HasAwaitTime s =
  ( HasType TimeSymbol POSIXTime (Input s)
  , HasType TimeSymbol WaitingForTime (Output s)
  , ContractRow s
  )

newtype WaitingForTime = WaitingForTime { unWaitingForTime :: POSIXTime }
  deriving stock (Eq, Ord, Show, Generic)
  deriving Pretty via (Tagged "WaitingForTime:" POSIXTime)
  deriving anyclass (ToJSON, FromJSON)

type AwaitTime = TimeSymbol .== (POSIXTime, WaitingForTime)

-- | A contract that waits until the time is reached, then returns the
--   current time.
awaitTime
    :: forall w s e.
       ( HasAwaitTime s
       , AsContractError e
       )
    => POSIXTime
    -> Contract w s e POSIXTime
awaitTime sl =
  let s = WaitingForTime sl
      check :: POSIXTime -> Maybe POSIXTime
      check sl' = if sl' >= sl then Just sl' else Nothing
  in
  requestMaybe @w @TimeSymbol @_ @_ @s s check

-- | Wait for a number of seconds.
waitNSeconds
  :: forall w s e.
     ( HasAwaitTime s
     , AsContractError e
     )
  => Integer
  -> Contract w s e POSIXTime
waitNSeconds i = do
  POSIXTime current <- currentTime
  awaitTime $ POSIXTime (current + i)

event
    :: forall s.
    ( HasType TimeSymbol POSIXTime (Input s)
    , AllUniqueLabels (Input s))
    => POSIXTime
    -> Event s
event = Event . IsJust #time

request
    :: forall s.
    ( HasType TimeSymbol WaitingForTime (Output s))
    => Handlers s
    -> Maybe POSIXTime
request (Handlers r) = unWaitingForTime <$> trial' r (Label @TimeSymbol)

-- | Run a contract until the given time has been reached.
until
  :: forall w s e a.
     ( HasAwaitTime s
     , AsContractError e
     )
  => Contract w s e a
  -> POSIXTime
  -> Contract w s e (Maybe a)
until c sl =
  fmap (either (const Nothing) Just) (selectEither (awaitTime @w @s sl) c)

-- | Run a contract when the given time has been reached.
when
  :: forall w s e a.
     ( HasAwaitTime s
     , AsContractError e
     )
  => POSIXTime
  -> Contract w s e a
  -> Contract w s e a
when s c = awaitTime @w @s s >> c

-- | Run a contract until the given time has been reached.
--   @timeout = flip until@
timeout
  :: forall w s e a.
     ( HasAwaitTime s
     , AsContractError e
     )
  => POSIXTime
  -> Contract w s e a
  -> Contract w s e (Maybe a)
timeout = flip (until @w @s)

-- | Wait until the first time is reached, then run the contract until
--   the second time is reached.
between
  :: forall w s e a.
     ( HasAwaitTime s
     , AsContractError e
     )
  => POSIXTime
  -> POSIXTime
  -> Contract w s e a
  -> Contract w s e (Maybe a)
between a b = timeout @w @s b . when @w @s a

-- | Repeatedly run a contract until the time is reached, then
--   return the last result.
collectUntil
  :: forall w s e a b.
     ( HasAwaitTime s
     , AsContractError e
     )
  => (a -> b -> b)
  -> b
  -> Contract w s e a
  -> POSIXTime
  -> Contract w s e b
collectUntil f b con s = foldMaybe f b (until @w @s con s)

-- | The current time
currentTime
  :: forall w s e.
  ( HasAwaitTime s
  , AsContractError e
  )
  => Contract w s e POSIXTime
currentTime = awaitTime 0
