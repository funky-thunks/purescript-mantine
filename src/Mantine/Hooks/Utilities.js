import hooks from '@mantine/hooks';

export const useDocumentTitleImpl      = hooks.useDocumentTitle;
export const useDocumentVisibilityImpl = hooks.useDocumentVisibility;
export const useFaviconImpl            = hooks.useFavicon;
export const usePageLeaveImpl          = hooks.usePageLeaveImpl;
export const useWindowScrollImpl       = hooks.useWindowScroll;

export const useHashImpl = () => {
  const [hash, setHash] = hooks.useHash;
  return { hash, setHash };
};

export const useWindowEventImpl = options => {
  hooks.useWindowScroll([ options.type, options.listener ]);
};
