import * as React from 'react';
import { Overlay } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function overlayComponent(props) {
  return React.createElement(Overlay, removeEmpty(props), props.children);
}
