module Mantine.Hooks.UIDom
  ( useClickOutside
  , UseClickOutside

  , useColorScheme
  , UseColorScheme

  , useElementSize
  , UseElementSize

  , useFocusReturn
  , UseFocusReturn
  , UseFocusReturnOptions

  , useFocusTrap
  , UseFocusTrap

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

  , useScrollIntoView
  , UseScrollIntoView
  , Alignment(..)
  , Axis(..)

  , useViewportSize
  , UseViewportSize
  , ViewportDimensions

  , useWindowEvent
  , UseWindowEvent

  , useWindowScroll
  , UseWindowScroll
  , Position
  ) where

import Mantine.Hooks.Prelude
import Mantine.Hooks.Theming (MantineColorScheme, MantineColorSchemeImpl)
import Mantine.Core.Common (ValueHandler, ValueHandlerImpl)

foreign import useClickOutsideImpl :: EffectFn2 (Effect Unit) (OptionalImpl (Array String)) (Ref Node)
foreign import data UseClickOutside :: Type -> Type

useClickOutside :: Effect Unit -> Optional (Array String) -> Hook UseClickOutside (Ref Node)
useClickOutside = mkHook2 useClickOutsideImpl

foreign import useColorSchemeImpl :: EffectFn2 (OptionalImpl MantineColorSchemeImpl) (OptionalImpl { getInitialValueInEffect :: Boolean }) String
foreign import data UseColorScheme :: Type -> Type

useColorScheme :: Optional MantineColorScheme -> Optional { getInitialValueInEffect :: Boolean } -> Hook UseColorScheme MantineColorScheme
useColorScheme = mkHook2 useColorSchemeImpl

foreign import useElementSizeImpl :: Effect { ref :: Ref Node, width :: Number, height :: Number }
foreign import data UseElementSize :: Type -> Type

useElementSize :: Hook UseElementSize { ref :: Ref Node, width :: Number, height :: Number }
useElementSize = mkHook0 useElementSizeImpl

foreign import useFocusReturnImpl :: EffectFn1 UseFocusReturnOptionsImpl (Effect Unit)
foreign import data UseFocusReturn :: Type -> Type

type UseFocusReturnOptions =
  { opened            :: Boolean
  , shouldReturnFocus :: Optional Boolean
  }

type UseFocusReturnOptionsImpl =
  { opened            :: Boolean
  , shouldReturnFocus :: OptionalImpl Boolean
  }

useFocusReturn :: UseFocusReturnOptions -> Hook UseFocusReturn (Effect Unit)
useFocusReturn = mkHook1 useFocusReturnImpl

foreign import useFocusTrapImpl :: EffectFn1 Boolean (Ref (Nullable Node))
foreign import data UseFocusTrap :: Type -> Type

useFocusTrap :: Boolean -> Hook UseFocusTrap (Ref (Nullable Node))
useFocusTrap = mkHook1 useFocusTrapImpl

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
  , options :: Optional { preventDefault :: Boolean }
  }

type HotkeyItemImpl =
  { hotkey  :: String
  , handler :: EffectFn1 KeyboardEvent Unit
  , options :: OptionalImpl { preventDefault :: Boolean }
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
  , initialValue :: Optional Boolean
  , options      :: Optional { getInitialValueInEffect :: Boolean }
  }

type UseMediaQueryOptionsImpl =
  { query        :: String
  , initialValue :: OptionalImpl Boolean
  , options      :: OptionalImpl { getInitialValueInEffect :: Boolean }
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

foreign import useReducedMotionImpl :: EffectFn2 (OptionalImpl Boolean) (OptionalImpl { getInitialValueInEffect :: Boolean }) Boolean
foreign import data UseReducedMotion :: Type -> Type

useReducedMotion :: Optional Boolean -> Optional { getInitialValueInEffect :: Boolean } -> Hook UseReducedMotion Boolean
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

foreign import useScrollIntoViewImpl :: EffectFn1 UseScrollIntoViewOptionsImpl UseScrollIntoViewResultImpl
foreign import data UseScrollIntoView :: Type -> Type

data Axis
  = AxisX
  | AxisY

type AxisImpl = String

instance ToFFI Axis AxisImpl where
  toNative = case _ of
    AxisX -> "x"
    AxisY -> "y"

type UseScrollIntoViewOptions =
  { onScrollFinish :: Effect Unit
  , duration       :: Optional Number
  , axis           :: Optional Axis
  , easing         :: Optional (Number -> Number)
  , offset         :: Optional Number
  , cancelable     :: Optional Boolean
  , isList         :: Optional Boolean
  }

type UseScrollIntoViewOptionsImpl =
  { onScrollFinish :: Effect Unit
  , duration       :: OptionalImpl Number
  , axis           :: OptionalImpl AxisImpl
  , easing         :: OptionalImpl (Number -> Number)
  , offset         :: OptionalImpl Number
  , cancelable     :: OptionalImpl Boolean
  , isList         :: OptionalImpl Boolean
  }

type UseScrollIntoViewResult =
  { targetRef      :: Ref (Nullable Node)
  , scrollableRef  :: Ref (Nullable Node)
  , scrollIntoView :: ValueHandler { alignment :: Optional Alignment }
  , cancel         :: Effect Unit
  }

data Alignment
  = AlignmentStart
  | AlignmentEnd
  | AlignmentCenter

type AlignmentImpl = String

instance ToFFI Alignment AlignmentImpl where
  toNative = case _ of
    AlignmentStart   -> "start"
    AlignmentEnd     -> "end"
    AlignmentCenter  -> "center"

type UseScrollIntoViewResultImpl =
  { targetRef      :: Ref (Nullable Node)
  , scrollableRef  :: Ref (Nullable Node)
  , scrollIntoView :: ValueHandlerImpl { alignment :: OptionalImpl AlignmentImpl }
  , cancel         :: Effect Unit
  }

useScrollIntoView :: UseScrollIntoViewOptions -> Hook UseScrollIntoView UseScrollIntoViewResult
useScrollIntoView = mkHook1 useScrollIntoViewImpl

type ViewportDimensions =
  { height :: Number
  , width  :: Number
  }

foreign import useViewportSizeImpl :: Effect ViewportDimensions

foreign import data UseViewportSize :: Type -> Type

useViewportSize :: Hook UseViewportSize ViewportDimensions
useViewportSize = mkHook0 useViewportSizeImpl

type UseWindowEventOptions =
  { type     :: String
  , listener :: Event -> Effect Unit
  }

type UseWindowEventOptionsImpl =
  { type     :: String
  , listener :: EffectFn1 Event Unit
  }

foreign import useWindowEventImpl :: EffectFn1 UseWindowEventOptionsImpl Unit
foreign import data UseWindowEvent :: Type -> Type

useWindowEvent :: UseWindowEventOptions -> Hook UseWindowEvent Unit
useWindowEvent = mkHook1 useWindowEventImpl

type UseWindowScrollImpl =
  { current :: Position
  , moveTo  :: EffectFn1 Position Unit
  }

type Position =
  { x :: Number
  , y :: Number
  }

foreign import useWindowScrollImpl :: Effect UseWindowScrollImpl
foreign import data UseWindowScroll :: Type -> Type

useWindowScroll :: Hook UseWindowScroll (Position /\ (Position -> Effect Unit))
useWindowScroll =
  let unpack { current, moveTo } = current /\ moveTo
   in unpack <$> mkHook0 useWindowScrollImpl
