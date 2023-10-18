import * as React from 'react';
import { CloseButton } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function closeButtonComponent(props) {
  return React.createElement(CloseButton, removeEmpty(props), props.children);
}
