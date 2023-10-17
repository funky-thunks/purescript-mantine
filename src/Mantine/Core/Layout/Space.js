import * as React from 'react';
import { Space } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function spaceComponent(props) {
  return React.createElement(Space, removeEmpty(props), props.children);
}
