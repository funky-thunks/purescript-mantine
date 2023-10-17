import * as React from 'react';
import { Drawer } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function drawerComponent(props) {
  return React.createElement(Drawer, removeEmpty(props), props.children);
}
