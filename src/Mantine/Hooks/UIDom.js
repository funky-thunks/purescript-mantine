import hooks from '@mantine/hooks';

export const useFocusWithinImpl  = hooks.useFocusWithin;
export const useFullscreenImpl   = hooks.useFullscreen;
export const useHoverImpl        = hooks.useHover;
export const useMediaQueryImpl   = hooks.useMediaQuery;
export const useMouseImpl        = hooks.useMouse;
export const useMoveImpl         = hooks.useMove;
export const useViewportSizeImpl = hooks.useViewportSize;

export const useHotkeysImpl = options => {
  hooks.useHotkeys([ options.hotKeyItems, options.tagsToIgnore ]);
};

export const useResizeObserverImpl = () => {
  const [ref, rect] = hooks.useResizeObserver;
  return { ref, rect };
};
