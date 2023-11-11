module Mantine.Hooks.UIDom
  ( useClickOutside
  , UseClickOutside

  , useColorScheme
  , UseColorScheme

  , useElementSize
  , UseElementSize

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

  , useMove
  , UseMove
  , UseMovePosition
  , UseMoveHandlers

  , useReducedMotion
  , UseReducedMotion

  , useResizeObserver
  , UseResizeObserver
  , ResizeRectangle

  , useViewportSize
  , UseViewportSize
  , ViewportDimensions
  ) where

import Mantine.Hooks.Prelude
import Mantine.Hooks.Theming (MantineColorScheme, MantineColorSchemeImpl)

foreign import useClickOutsideImpl :: EffectFn2 (Effect Unit) (Nullable (Array String)) (Ref Node)
foreign import data UseClickOutside :: Type -> Type

useClickOutside :: Effect Unit -> Maybe (Array String) -> Hook UseClickOutside (Ref Node)
useClickOutside = mkHook2 useClickOutsideImpl

foreign import useColorSchemeImpl :: EffectFn2 (Nullable MantineColorSchemeImpl) (Nullable { getInitialValueInEffect :: Boolean }) String
foreign import data UseColorScheme :: Type -> Type

useColorScheme :: Maybe MantineColorScheme -> Maybe { getInitialValueInEffect :: Boolean } -> Hook UseColorScheme MantineColorScheme
useColorScheme = mkHook2 useColorSchemeImpl

foreign import useElementSizeImpl :: Effect { ref :: Ref Node, width :: Number, height :: Number }
foreign import data UseElementSize :: Type -> Type

useElementSize :: Hook UseElementSize { ref :: Ref Node, width :: Number, height :: Number }
useElementSize = mkHook0 useElementSizeImpl

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
  let unpack { focused, ref } = focused /\ ref
   in unpack <$> mkHook1 useFocusWithinImpl handlers

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

foreign import useFullscreenImpl :: Effect UseFullscreenResultImpl

foreign import data UseFullscreen :: Type -> Type

useFullscreen :: Hook UseFullscreen UseFullscreenResult
useFullscreen = mkHook0 useFullscreenImpl

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
useHotkeys = mkHook1 useHotkeysImpl

foreign import useHoverImpl :: Effect { ref :: Ref Node, hovered :: Boolean }
foreign import data UseHover :: Type -> Type

useHover :: Hook UseHover { ref :: Ref Node, hovered :: Boolean }
useHover = mkHook0 useHoverImpl

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
useMediaQuery = mkHook1 useMediaQueryImpl

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
useMouse = mkHook1 useMouseImpl

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
  let unpack { active, ref } = active /\ ref
   in unpack <$> mkHook2 useMoveImpl onChange handlers

foreign import useReducedMotionImpl :: EffectFn2 (Nullable Boolean) (Nullable { getInitialValueInEffect :: Boolean }) Boolean
foreign import data UseReducedMotion :: Type -> Type

useReducedMotion :: Maybe Boolean -> Maybe { getInitialValueInEffect :: Boolean } -> Hook UseReducedMotion Boolean
useReducedMotion = mkHook2 useReducedMotionImpl

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
  let unpack { ref, rect } = ref /\ rect
   in unpack <$> mkHook0 useResizeObserverImpl

type ViewportDimensions =
  { height :: Number
  , width  :: Number
  }

foreign import useViewportSizeImpl :: Effect ViewportDimensions

foreign import data UseViewportSize :: Type -> Type

useViewportSize :: Hook UseViewportSize ViewportDimensions
useViewportSize = mkHook0 useViewportSizeImpl
