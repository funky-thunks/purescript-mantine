import * as React from 'react';
import { Blockquote } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function blockquoteComponent(props) {
  return React.createElement(Blockquote, removeEmpty(props), props.children);
}
