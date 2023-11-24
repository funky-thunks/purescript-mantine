module Mantine.Core.PartialRecords
  ( partial

  , class Partial
  , partial'
  , class RecordPartial
  , recordPartial

  , example
  ) where

import Prelude (pure)
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (class MonoidRecord, mempty)
import Data.Symbol (class IsSymbol)
import Mantine.FFI (Optional(..))
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Record (delete, get, insert, union)
import Type.Proxy (Proxy(..))

type ExampleRecord = { foo :: Optional String, bar :: Optional Int }

example :: ExampleRecord
example = partial { foo: "string" }

partial :: forall attrs          attrs_          props
                  undefinedAttrs undefinedAttrs_ undefinedProps  undefinedAttrs_List
         . Union  attrs          attrs_          props
        => Union  undefinedAttrs  undefinedAttrs_ undefinedProps
        => Partial (Record attrs ) (Record undefinedAttrs)
        => RowToList undefinedAttrs_ undefinedAttrs_List
        => MonoidRecord  undefinedAttrs_List undefinedAttrs_ undefinedAttrs_
        => Record attrs -> Record undefinedProps
partial attrs = partial' attrs `union` mempty

class Partial attrs partial | attrs -> partial where
  partial' :: attrs -> partial

instance ( RowToList definedFields definedFieldList
         , RecordPartial definedFieldList definedFields undefinedFields
         ) => Partial (Record definedFields) (Record undefinedFields) where
 partial' = recordPartial Proxy

class RecordPartial
  (definedFieldList :: RowList Type) (definedFields :: Row Type) (undefinedFields :: Row Type)
  | definedFieldList -> definedFields
  , definedFieldList -> undefinedFields where
  recordPartial
    :: RowToList definedFields definedFieldList
    => Proxy definedFieldList
    -> Record definedFields
    -> Record undefinedFields

instance ( IsSymbol key
         , Cons     key           defined    definedRecordTail   definedRecord
         , Cons     key (Optional defined) undefinedRecordTail undefinedRecord
         , Lacks    key   definedRecordTail
         , Lacks    key undefinedRecordTail
         , RecordPartial definedTail definedRecordTail undefinedRecordTail
         , RowToList definedRecordTail definedTail
         ) => RecordPartial (Cons key defined definedTail) definedRecord undefinedRecord where
  recordPartial _ obj =
    let name = Proxy :: Proxy key
        head = pure (get name obj)
        tail = recordPartial Proxy (delete name obj)
     in insert name head tail

instance RecordPartial Nil any () where
  recordPartial _ _ = {}
