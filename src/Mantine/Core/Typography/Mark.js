import * as React from 'react';
import { Mark } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function markComponent(props) {
  return React.createElement(Mark, removeEmpty(props), props.children);
}
