import * as React from 'react';
import { LoadingOverlay } from '@mantine/core';

export function loadingOverlayComponent(props) {
  return React.createElement(LoadingOverlay, props, props.children);
}
