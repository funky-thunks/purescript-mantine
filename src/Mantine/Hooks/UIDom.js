import hooks from '@mantine/hooks';

export const useClickOutsideImpl   = hooks.useClickOutside;
export const useColorSchemeImpl    = hooks.useColorScheme;
export const useElementSizeImpl    = hooks.useElementSize;
export const useFocusReturnImpl    = hooks.useFocusReturn;
export const useFocusTrapImpl      = hooks.useFocusTrap;
export const useFocusWithinImpl    = hooks.useFocusWithin;
export const useFullscreenImpl     = hooks.useFullscreen;
export const useHoverImpl          = hooks.useHover;
export const useMediaQueryImpl     = hooks.useMediaQuery;
export const useMouseImpl          = hooks.useMouse;
export const useMoveImpl           = hooks.useMove;
export const useReducedMotionImpl  = hooks.useReducedMotion;
export const useScrollIntoViewImpl = hooks.useScrollIntoView;
export const useViewportSizeImpl   = hooks.useViewportSize;
export const useWindowScrollImpl   = hooks.useWindowScroll;

export const useHotkeysImpl = options => {
  hooks.useHotkeys([ options.hotKeyItems, options.tagsToIgnore ]);
};

export const useResizeObserverImpl = () => {
  const [ref, rect] = hooks.useResizeObserver;
  return { ref, rect };
};

export const useWindowEventImpl = options => {
  hooks.useWindowScroll([ options.type, options.listener ]);
};
