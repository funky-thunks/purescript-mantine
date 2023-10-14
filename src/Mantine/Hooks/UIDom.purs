module Mantine.Hooks.UIDom
  ( useClickOutside
  , UseClickOutside

  , useColorScheme
  , UseColorScheme
  , ColorScheme(..)

  , useFocusWithin
  , UseFocusWithin

  , useFullscreen
  , UseFullscreen
  , UseFullscreenResult

  , useHotkeys
  , UseHotkeys
  , UseHotkeysOptions
  , HotkeyItem

  , useHover
  , UseHover

  , useMediaQuery
  , UseMediaQuery
  , UseMediaQueryOptions

  , useMouse
  , useMouse_
  , UseMouse
  , UseMouseOptions
  , UseMouseResult

  , UseMove
  , UseMovePosition
  , UseMoveHandlers
  , useMove

  , useResizeObserver
  , UseResizeObserver
  , ResizeRectangle

  , useViewportSize
  , UseViewportSize
  , ViewportDimensions
  ) where

import Prelude
import Control.Promise (Promise, toAff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import React.Basic.Hooks (type (/\), Hook, Ref, (/\), unsafeHook)
import Web.DOM (Node)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

foreign import useClickOutsideImpl :: EffectFn2 (Effect Unit) (Nullable (Array String)) (Ref Node)
foreign import data UseClickOutside :: Type -> Type

useClickOutside :: Effect Unit -> Maybe (Array String) -> Hook UseClickOutside (Ref Node)
useClickOutside handler events = unsafeHook (runEffectFn2 useClickOutsideImpl handler (toNullable events))

foreign import useColorSchemeImpl :: EffectFn2 (Nullable String) (Nullable { getInitialValueInEffect :: Boolean }) String
foreign import data UseColorScheme :: Type -> Type

data ColorScheme = Light | Dark

useColorScheme :: Maybe ColorScheme -> Maybe { getInitialValueInEffect :: Boolean } -> Hook UseColorScheme ColorScheme
useColorScheme initialValue options =
  let fromColorScheme = case _ of
        Dark  -> "dark"
        Light -> "light"
      toColorScheme = case _ of
        "dark" -> Dark
        _      -> Light
   in unsafeHook (toColorScheme <$> runEffectFn2 useColorSchemeImpl (toNullable (fromColorScheme <$> initialValue)) (toNullable options))

type UseFocusWithinHandlers =
  { onFocus :: Effect Unit
  , onBlur  :: Effect Unit
  }

type UseFocusWithinResult =
  { focused :: Boolean
  , ref     :: Ref (Nullable Node)
  }

foreign import useFocusWithinImpl :: EffectFn1 UseFocusWithinHandlers UseFocusWithinResult

foreign import data UseFocusWithin :: Type -> Type

useFocusWithin ::  UseFocusWithinHandlers -> Hook UseFocusWithin (Boolean /\ Ref (Nullable Node))
useFocusWithin handlers =
  let mkResult { focused, ref } = focused /\ ref
   in unsafeHook (mkResult <$> runEffectFn1 useFocusWithinImpl handlers)

type UseFullscreenResultImpl =
  { fullscreen :: Boolean
  , ref        :: Ref (Nullable Node)
  , toggle     :: Promise Unit
  }

type UseFullscreenResult =
  { fullscreen :: Boolean
  , ref        :: Ref (Nullable Node)
  , toggle     :: Aff Unit
  }

fromUseFullscreenResultImpl :: UseFullscreenResultImpl -> UseFullscreenResult
fromUseFullscreenResultImpl n = n { toggle = toAff n.toggle }

foreign import useFullscreenImpl :: Effect UseFullscreenResultImpl

foreign import data UseFullscreen :: Type -> Type

useFullscreen :: Hook UseFullscreen UseFullscreenResult
useFullscreen = unsafeHook (fromUseFullscreenResultImpl <$> useFullscreenImpl)

type UseHotkeysOptions item =
  { hotKeyItems  :: Array item
  , tagsToIgnore :: Array String
  }

type HotkeyItem =
  { hotkey  :: String
  , handler :: KeyboardEvent -> Effect Unit
  , options :: Maybe { preventDefault :: Boolean }
  }

type HotkeyItemImpl =
  { hotkey  :: String
  , handler :: EffectFn1 KeyboardEvent Unit
  , options :: Nullable { preventDefault :: Boolean }
  }

foreign import useHotkeysImpl :: EffectFn1 (UseHotkeysOptions HotkeyItemImpl) Unit
foreign import data UseHotkeys :: Type -> Type

useHotkeys :: UseHotkeysOptions HotkeyItem -> Hook UseHotkeys Unit
useHotkeys options =
  let nativeOptions = options { hotKeyItems = nativeHotKeyItem <$> options.hotKeyItems}
      nativeHotKeyItem item = item { options = toNullable item.options, handler = mkEffectFn1 item.handler }
   in unsafeHook (runEffectFn1 useHotkeysImpl nativeOptions)

foreign import useHoverImpl :: Effect { ref :: Ref Node, hovered :: Boolean }
foreign import data UseHover :: Type -> Type

useHover :: Hook UseHover { ref :: Ref Node, hovered :: Boolean }
useHover = unsafeHook useHoverImpl

type UseMediaQueryOptions =
  { query        :: String
  , initialValue :: Maybe Boolean
  , options      :: Maybe { getInitialValueInEffect :: Boolean }
  }

type UseMediaQueryOptionsImpl =
  { query        :: String
  , initialValue :: Nullable Boolean
  , options      :: Nullable { getInitialValueInEffect :: Boolean }
  }

foreign import useMediaQueryImpl :: EffectFn1 UseMediaQueryOptionsImpl Boolean

foreign import data UseMediaQuery :: Type -> Type

useMediaQuery :: UseMediaQueryOptions -> Hook UseMediaQuery Boolean
useMediaQuery options =
  let nativeOptions = options { initialValue = toNullable options.initialValue, options = toNullable options.options }
   in unsafeHook (runEffectFn1 useMediaQueryImpl nativeOptions)

type UseMouseOptions =
  { resetOnExit :: Boolean
  }

type UseMouseResult =
  { x   :: Number
  , y   :: Number
  , ref :: Ref (Nullable Node)
  }

foreign import useMouseImpl :: EffectFn1 UseMouseOptions UseMouseResult

foreign import data UseMouse :: Type -> Type

useMouse :: UseMouseOptions -> Hook UseMouse UseMouseResult
useMouse options = unsafeHook (runEffectFn1 useMouseImpl options)

useMouse_ :: Hook UseMouse UseMouseResult
useMouse_ = useMouse { resetOnExit: false }

type UseMovePosition =
  { x :: Number
  , y :: Number
  }

type UseMoveHandlers =
  { onScrubStart :: Effect Unit
  , onScrubEnd   :: Effect Unit
  }

type UseMoveResult =
  { active :: Boolean
  , ref    :: Ref (Nullable Node)
  }

foreign import useMoveImpl :: EffectFn2 (EffectFn1 UseMovePosition Unit) UseMoveHandlers UseMoveResult

foreign import data UseMove :: Type -> Type

useMove :: (UseMovePosition -> Effect Unit) -> UseMoveHandlers -> Hook UseMove (Boolean /\ Ref (Nullable Node))
useMove onChange handlers =
  let mkResult { active, ref } = active /\ ref
   in unsafeHook (mkResult <$> runEffectFn2 useMoveImpl (mkEffectFn1 onChange) handlers)

type ResizeRectangle =
  { x      :: Number
  , y      :: Number
  , top    :: Number
  , left   :: Number
  , right  :: Number
  , bottom :: Number
  , height :: Number
  , width  :: Number
  }

type UseResizeObserverImpl =
  { ref  :: Ref Node
  , rect :: ResizeRectangle
  }

foreign import useResizeObserverImpl :: Effect UseResizeObserverImpl
foreign import data UseResizeObserver :: Type -> Type

useResizeObserver :: Hook UseResizeObserver (Ref Node /\ ResizeRectangle)
useResizeObserver =
  let fromNative { ref, rect } = ref /\ rect
   in unsafeHook (fromNative <$> useResizeObserverImpl)

type ViewportDimensions =
  { height :: Number
  , width  :: Number
  }

foreign import useViewportSizeImpl :: Effect ViewportDimensions

foreign import data UseViewportSize :: Type -> Type

useViewportSize :: Hook UseViewportSize ViewportDimensions
useViewportSize = unsafeHook  useViewportSizeImpl
