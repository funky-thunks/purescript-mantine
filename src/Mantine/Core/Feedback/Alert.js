import * as React from 'react';
import { Alert } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function alertComponent(props) {
  return React.createElement(Alert, removeEmpty(props), props.children);
}
