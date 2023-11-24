module Examples.Core.Prelude
  ( module Prelude
  , module Data.Array
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Monoid
  , module Data.String
  , module React.Basic
  , module React.Basic.DOM.Events
  , module React.Basic.Events
  , module React.Basic.Hooks
  , module Type.Proxy
  ) where

import Prelude -- (Unit, const, discard, identity, map, mempty, pure, unit, ($), (=<<), (<<<), (>>>), (<$), (<$>), (==), (/=), (<>), (&&), (||))
import Data.Array (elem, filter, last)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard, memptyRecord)
import Data.String (length)
import React.Basic (JSX)
import React.Basic.DOM.Events (nativeEvent, preventDefault)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, (/\), component)
import Type.Proxy (Proxy(..))
