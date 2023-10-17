import * as React from 'react';
import { LoadingOverlay } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function loadingOverlayComponent(props) {
  return React.createElement(LoadingOverlay, removeEmpty(props), props.children);
}
