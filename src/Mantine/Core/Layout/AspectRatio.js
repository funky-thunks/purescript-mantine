import * as React from 'react';
import { AspectRatio } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function aspectRatioComponent(props) {
  return React.createElement(AspectRatio, removeEmpty(props), props.children);
}
