module Mantine.Core.PartialRecords
  ( partial

  , class PartialRecord
  , partial'
  , class RecordPartialRecord
  , recordPartial
  ) where

import Prelude (pure)
import Data.Monoid (class MonoidRecord, mempty)
import Data.Symbol (class IsSymbol)
import Mantine.FFI (Optional)
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Record (delete, get, insert, union)
import Type.Proxy (Proxy(..))

partial :: forall attrs          attrs_          props
                  undefinedAttrs undefinedAttrs_ undefinedProps  undefinedAttrs_List
         . Union  attrs          attrs_          props
        => Union  undefinedAttrs  undefinedAttrs_ undefinedProps
        => PartialRecord (Record attrs) (Record undefinedAttrs)
        => RowToList undefinedAttrs_ undefinedAttrs_List
        => MonoidRecord  undefinedAttrs_List undefinedAttrs_ undefinedAttrs_
        => Record attrs -> Record undefinedProps
partial attrs = partial' attrs `union` mempty

class PartialRecord attrs partial | attrs -> partial where
  partial' :: attrs -> partial

instance ( RowToList definedFields definedFieldList
         , RecordPartialRecord definedFieldList definedFields undefinedFields
         ) => PartialRecord (Record definedFields) (Record undefinedFields) where
 partial' = recordPartial Proxy

class RecordPartialRecord
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
         , RecordPartialRecord definedTail definedRecordTail undefinedRecordTail
         , RowToList definedRecordTail definedTail
         ) => RecordPartialRecord (Cons key defined definedTail) definedRecord undefinedRecord where
  recordPartial _ obj =
    let name = Proxy :: Proxy key
        head = pure (get name obj)
        tail = recordPartial Proxy (delete name obj)
     in insert name head tail

instance RecordPartialRecord Nil any () where
  recordPartial _ _ = {}
