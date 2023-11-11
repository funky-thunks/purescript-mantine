import hooks from '@mantine/hooks';

export const useClipboardImpl          = hooks.useClipboard;
export const useDocumentTitleImpl      = hooks.useDocumentTitle;
export const useDocumentVisibilityImpl = hooks.useDocumentVisibility;
export const useFaviconImpl            = hooks.useFavicon;
export const useHeadroomImpl           = hooks.useHeadroom;
export const useIdleImpl               = hooks.useIdle;
export const useNetworkImpl            = hooks.useNetwork;
export const usePageLeaveImpl          = hooks.usePageLeaveImpl;

export const useHashImpl = () => {
  const [hash, setHash] = hooks.useHash;
  return { hash, setHash };
};
