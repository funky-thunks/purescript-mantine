module Mantine.FFI
  ( class ToFFI
  , toNative
  , class RecordToFFI
  , recordToNative

  , Optional(..)
  , OptionalImpl
  , toOptionalImpl

  , class FromFFI
  , fromNative
  , class RecordFromFFI
  , recordFromNative
  ) where

import Prelude (Unit, identity, map, (<<<))
import Control.Applicative (class Applicative, class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Promise (Promise, toAff)
import Data.Either (Either, either)
import Data.Functor (class Functor)
import Data.Int (floor, toNumber)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Semigroup (class Semigroup)
import Data.Show (class Show)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1)
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import React.Basic (Ref)
import React.Basic.DOM (CSS)
import React.Basic.Emotion (Style)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX)
import Record (delete, get, insert)
import Type.Proxy (Proxy(..))
import Untagged.Union (class InOneOf, type (|+|), UndefinedOr, asOneOf, maybeToUor, uorToMaybe)
import Web.Event.Event (Event)
import Web.File.File (File)
import Web.HTML (HTMLElement)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

class ToFFI ps js | ps -> js where
  toNative :: ps -> js

instance ToFFI Unit Unit where
  toNative = identity

instance ToFFI Boolean Boolean where
  toNative = identity

instance ToFFI Char Char where
  toNative = identity

instance ToFFI Int Number where
  toNative = toNumber

instance ToFFI Number Number where
  toNative = identity

instance ToFFI String String where
  toNative = identity

instance ToFFI Foreign Foreign where
  toNative = identity

instance ToFFI abstract native => ToFFI (Array abstract) (Array native) where
  toNative = map toNative

instance ToFFI abstract native => ToFFI (Maybe abstract) (Nullable native) where
  toNative m = toNullable (map toNative m)

newtype Optional v = Optional (Maybe v)

derive newtype instance Show v => Show (Optional v)
derive newtype instance Functor       Optional
derive newtype instance Apply         Optional
derive newtype instance Applicative   Optional
derive newtype instance Bind          Optional
derive newtype instance Monad         Optional

instance Semigroup (Optional v) where
  append (Optional (Just v)) _ = Optional (Just v)
  append (Optional _       ) o = o

instance Monoid (Optional v) where
  mempty = Optional Nothing

type OptionalImpl v = UndefinedOr v

toOptionalImpl :: forall v. Optional v -> OptionalImpl v
toOptionalImpl (Optional m) = maybeToUor m

instance ToFFI abstract native => ToFFI (Optional abstract) (OptionalImpl native) where
  toNative = toOptionalImpl <<< map toNative

fromOptionalImpl :: forall v. OptionalImpl v -> Optional v
fromOptionalImpl = Optional <<< uorToMaybe

instance FromFFI native abstract => FromFFI (OptionalImpl native) (Optional abstract) where
  fromNative = map fromNative <<< fromOptionalImpl

instance ToFFI abstract native => ToFFI (Object abstract) (Object native) where
  toNative = map toNative

instance ToFFI CSS CSS where
  toNative = identity

instance ToFFI JSX JSX where
  toNative = identity

instance ToFFI HTMLElement HTMLElement where
  toNative = identity

instance ToFFI File File where
  toNative = identity

instance ToFFI Style Style where
  toNative = identity

instance ToFFI Event Event where
  toNative = identity

instance ToFFI KeyboardEvent KeyboardEvent where
  toNative = identity

instance ToFFI MouseEvent MouseEvent where
  toNative = identity

instance ToFFI EventHandler EventHandler where
  toNative = identity

instance ToFFI (Ref referenced) (Ref referenced) where
  toNative = identity

instance ToFFI result native => ToFFI (Effect result) (Effect native) where
  toNative = map toNative

instance (FromFFI arg0JS arg0PS, ToFFI resultPS resultJS) => ToFFI (arg0PS -> Effect resultPS) (EffectFn1 arg0JS resultJS) where
  toNative f = mkEffectFn1 (toNative <<< f <<< fromNative)
else instance (FromFFI arg0JS arg0PS, FromFFI arg1JS arg1PS, ToFFI resultPS resultJS) => ToFFI (arg0PS -> arg1PS -> Effect resultPS) (EffectFn2 arg0JS arg1JS resultJS) where
  toNative f = mkEffectFn2 (\ arg0 arg1 -> toNative (f (fromNative arg0) (fromNative arg1)))
else instance (FromFFI arg0JS arg0PS, ToFFI resultPS resultJS) => ToFFI (arg0PS -> resultPS) (arg0JS -> resultJS) where
  toNative f = toNative <<< f <<< fromNative

instance ToFFI JSDate JSDate where
  toNative = identity

instance ( InOneOf nativeRight nativeLeft nativeRight
         , ToFFI abstractLeft  nativeLeft
         , ToFFI abstractRight nativeRight
         ) => ToFFI (Either abstractLeft abstractRight) (nativeLeft |+| nativeRight) where
  toNative = either (asOneOf <<< toNative) (asOneOf <<< toNative)

instance ( RowToList abstractFields abstractFieldList
         , RecordToFFI abstractFieldList abstractFields nativeFields
         ) => ToFFI (Record abstractFields) (Record nativeFields) where
 toNative = recordToNative Proxy

class RecordToFFI (abstractFieldList :: RowList Type) (abstractFields :: Row Type) (nativeFields :: Row Type)
                  | abstractFieldList -> abstractFields
                  , abstractFieldList -> nativeFields   where
  recordToNative
    :: RowToList abstractFields abstractFieldList
    => Proxy abstractFieldList
    -> Record abstractFields
    -> Record nativeFields

instance ( IsSymbol key
         , Cons     key abstract abstractRecordTail abstractRecord
         , Cons     key native   nativeRecordTail   nativeRecord
         , Lacks    key nativeRecordTail
         , Lacks    key abstractRecordTail
         , ToFFI abstract native
         , RecordToFFI abstractTail abstractRecordTail nativeRecordTail
         , RowToList abstractRecordTail abstractTail
         ) => RecordToFFI (Cons key abstract abstractTail) abstractRecord nativeRecord where
  recordToNative _ obj =
    let name = Proxy :: Proxy key
        head = toNative (get name obj)
        tail = recordToNative Proxy (delete name obj)
     in insert name head tail

instance RecordToFFI Nil any () where
  recordToNative _ _ = {}

class FromFFI js ps | ps -> js where
  fromNative :: js -> ps

instance FromFFI Unit Unit where
  fromNative = identity

instance FromFFI Boolean Boolean where
  fromNative = identity

instance FromFFI Char Char where
  fromNative = identity

instance FromFFI Number Int where
  fromNative = floor

instance FromFFI Number Number where
  fromNative = identity

instance FromFFI String String where
  fromNative = identity

instance FromFFI Foreign Foreign where
  fromNative = identity

instance FromFFI native abstract => FromFFI (Array native) (Array abstract) where
  fromNative = map fromNative

instance FromFFI native abstract => FromFFI (Nullable native) (Maybe abstract) where
  fromNative m = map fromNative (toMaybe m)

instance FromFFI native abstract => FromFFI (Object native) (Object abstract) where
  fromNative = map fromNative

instance FromFFI JSX JSX where
  fromNative = identity

instance FromFFI HTMLElement HTMLElement where
  fromNative = identity

instance FromFFI File File where
  fromNative = identity

instance FromFFI Style Style where
  fromNative = identity

instance FromFFI Event Event where
  fromNative = identity

instance FromFFI KeyboardEvent KeyboardEvent where
  fromNative = identity

instance FromFFI MouseEvent MouseEvent where
  fromNative = identity

instance FromFFI EventHandler EventHandler where
  fromNative = identity

instance FromFFI (Ref referenced) (Ref referenced) where
  fromNative = identity

instance FromFFI native result => FromFFI (Effect native) (Effect result) where
  fromNative = map fromNative

instance (FromFFI resultJS resultPS, ToFFI arg0PS arg0JS) => FromFFI (EffectFn1 arg0JS resultJS) (arg0PS -> Effect resultPS) where
  fromNative vh = map fromNative <<< runEffectFn1 vh <<< toNative

instance FromFFI JSDate JSDate where
  fromNative = identity

instance FromFFI js ps => FromFFI (Promise js) (Aff ps) where
  fromNative = map fromNative <<< toAff

instance FromFFI Error Error where
  fromNative = identity

instance ( RowToList     nativeFields    nativeFieldList
         , RecordFromFFI nativeFieldList nativeFields    abstractFields
         ) => FromFFI (Record nativeFields) (Record abstractFields) where
 fromNative = recordFromNative Proxy

class RecordFromFFI (nativeFieldList :: RowList Type) (nativeFields :: Row Type) (abstractFields :: Row Type)
                    | nativeFieldList -> nativeFields
                    , nativeFieldList -> abstractFields where
  recordFromNative
    :: RowToList nativeFields nativeFieldList
    => Proxy nativeFieldList
    -> Record nativeFields
    -> Record abstractFields

instance ( IsSymbol key
         , Cons     key native   nativeRecordTail   nativeRecord
         , Cons     key abstract abstractRecordTail abstractRecord
         , Lacks    key abstractRecordTail
         , Lacks    key nativeRecordTail
         , FromFFI native abstract
         , RecordFromFFI nativeTail nativeRecordTail abstractRecordTail
         , RowToList nativeRecordTail nativeTail
         ) => RecordFromFFI (Cons key native nativeTail) nativeRecord abstractRecord where
  recordFromNative _ obj =
    let name = Proxy :: Proxy key
        head = fromNative (get name obj)
        tail = recordFromNative Proxy (delete name obj)
     in insert name head tail

instance RecordFromFFI Nil any () where
  recordFromNative _ _ = {}
