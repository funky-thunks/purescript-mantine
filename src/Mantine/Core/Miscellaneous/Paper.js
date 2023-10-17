import * as React from 'react';
import { Paper } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function paperComponent(props) {
  return React.createElement(Paper, removeEmpty(props), props.children);
}
