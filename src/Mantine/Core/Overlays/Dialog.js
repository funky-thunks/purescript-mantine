import * as React from 'react';
import { Dialog } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function dialogComponent(props) {
  return React.createElement(Dialog, removeEmpty(props), props.children);
}
