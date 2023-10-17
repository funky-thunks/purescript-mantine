import * as React from 'react';
import { Divider } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function dividerComponent(props) {
  return React.createElement(Divider, removeEmpty(props), props.children);
}
