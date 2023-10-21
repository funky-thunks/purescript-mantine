import * as React from 'react';
import { CopyButton } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function copyButtonComponent(props) {
  return React.createElement(CopyButton, removeEmpty(props), props.children);
}
