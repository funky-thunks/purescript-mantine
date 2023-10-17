import * as React from 'react';
import { SimpleGrid } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function simpleGridComponent(props) {
  return React.createElement(SimpleGrid, removeEmpty(props), props.children);
}
