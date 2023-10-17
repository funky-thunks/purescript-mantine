import * as React from 'react';
import { ScrollArea } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function scrollAreaComponent(props) {
  return React.createElement(ScrollArea, removeEmpty(props), props.children);
}
