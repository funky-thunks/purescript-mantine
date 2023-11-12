module Mantine.Core.Prelude
  ( module Prelude
  , module Data.Default
  , module Data.Either
  , module Data.Maybe
  , module Data.Nullable
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Effect
  , module Effect.Uncurried
  , module Foreign.Object
  , module Mantine.Core.CSS
  , module Mantine.Core.Common
  , module Mantine.FFI
  , module React.Basic
  , module React.Basic.Events
  , module React.Basic.Hooks
  , module Record
  , module Type.Proxy
  , module Type.Row
  , module Untagged.Union
  ) where

import Prelude (Unit, const, identity, map, mempty, pure, unit, ($), (=<<), (<<<), (>>>), (<$), (<$>))
import Data.Default (class DefaultValue, defaultValue)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull, null, toMaybe, toNullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Foreign.Object (Object, fromFoldable)
import Mantine.Core.CSS (AlignContent(..), AlignContentImpl, AlignItems(..), AlignItemsImpl, FlexDirection(..), FlexDirectionImpl, FlexWrap(..), FlexWrapImpl, FontWeight(..), FontWeightImpl, GlobalValues(..), JustifyContent(..), JustifyContentImpl, ListStyleType(..), ListStyleTypeImpl, ObjectFit(..), ObjectFitImpl, PointerEvents(..), PointerEventsImpl, Position(..), PositionImpl, TableLayout(..), TableLayoutImpl, TextAlign(..), TextAlignImpl)
import Mantine.Core.Common (CheckerHandler(..), CheckerHandlerImpl, Controlled, ControlledImpl, ControlledImpl_, Controlled_, Degrees(..), DegreesImpl, Dimension(..), DimensionImpl, DimmedOrColor(..), DimmedOrColorImpl, FixedOrResponsive, FixedOrResponsiveImpl, InputHandler(..), InputHandlerImpl, MantineColor(..), MantineColorImpl, MantineComponent, MantineComponentImpl, MantineComponentImplRow, MantineComponentRow, MantineGradient, MantineGradientImpl, MantineNumberSize(..), MantineNumberSizeImpl, MantineShadow, MantineShadowImpl, MantineSize(..), MantineSizeImpl, MantineSpacing, MantineSpacingImpl, MantineTransition(..), MantineTransitionBase, MantineTransitionBaseImpl, MantineTransitionImpl, MantineTransitionProps, MantineTransitionPropsImpl, MantineTransitionTimingFunction(..), MantineTransitionTimingFunctionImpl, Milliseconds, MillisecondsImpl, Orientation(..), OrientationImpl, Pixels, PixelsImpl, Polymorphic, PolymorphicImpl, PopoverMiddlewares, PopoverMiddlewaresImpl, Radius(..), RadiusImpl, Rem, RemImpl, Responsive, ResponsiveImpl, ValueHandler(..), ValueHandlerImpl, ZIndex, ZIndexImpl, defaultMantineComponent, defaultMantineComponent_, mkComponent, mkComponentWithDefault, mkTrivialComponent)
import Mantine.FFI (class FromFFI, class ToFFI, fromNative, toNative)
import React.Basic (ReactComponent, Ref)
import React.Basic.Events (EventHandler, SyntheticEvent, handler, handler_)
import React.Basic.Hooks (JSX)
import Record (delete, merge, rename, union)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Untagged.Union (type (|+|), UndefinedOr, asOneOf)
