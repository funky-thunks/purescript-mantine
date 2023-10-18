import * as React from 'react';
import { Highlight } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function highlightComponent(props) {
  return React.createElement(Highlight, removeEmpty(props), props.children);
}
