import * as React from 'react';
import { Progress } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function progressComponent(props) {
  return React.createElement(Progress, removeEmpty(props), props.children);
}
