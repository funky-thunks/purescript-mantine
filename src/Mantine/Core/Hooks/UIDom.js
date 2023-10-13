import hooks from '@mantine/hooks';

export const useFocusWithinImpl  = hooks.useFocusWithin;
export const useFullscreenImpl   = hooks.useFullscreen;
export const useMediaQueryImpl   = hooks.useMediaQuery;
export const useMouseImpl        = hooks.useMouse;
export const useMoveImpl         = hooks.useMove;
export const useViewportSizeImpl = hooks.useViewportSize;

export const useHotkeysImpl = options => {
  hooks.useHotkeys([ options.hotKeyItems, options.tagsToIgnore ]);
};
