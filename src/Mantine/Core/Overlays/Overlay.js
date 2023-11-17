import * as React from 'react';
import { Overlay } from '@mantine/core';

export function overlayComponent(props) {
  return React.createElement(Overlay, props, props.children);
}
