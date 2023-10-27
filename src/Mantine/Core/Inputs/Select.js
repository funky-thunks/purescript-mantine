import * as React from 'react';
import { Select } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function selectComponent(props) {
  return React.createElement(Select, removeEmpty(props), props.children);
}
