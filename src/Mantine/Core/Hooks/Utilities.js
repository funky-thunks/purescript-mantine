import hooks from '@mantine/hooks';

export const useDocumentTitleImpl = hooks.useDocumentTitle;
export const useFaviconImpl       = hooks.useFavicon;
export const usePageLeaveImpl     = hooks.usePageLeaveImpl;
export const useHashImpl          = () => {
  const [hash, setHash] = hooks.useHash;
  return { hash, setHash };
};
