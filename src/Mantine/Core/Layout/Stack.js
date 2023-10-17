import * as React from 'react';
import { Stack } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function stackComponent(props) {
  return React.createElement(Stack, removeEmpty(props), props.children);
}
