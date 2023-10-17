import * as React from 'react';
import { RingProgress } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function ringProgressComponent(props) {
  return React.createElement(RingProgress, removeEmpty(props), props.children);
}
