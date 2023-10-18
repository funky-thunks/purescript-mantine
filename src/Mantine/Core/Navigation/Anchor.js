import * as React from 'react';
import { Anchor } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function anchorComponent(props) {
  return React.createElement(Anchor, removeEmpty(props), props.children);
}
