import * as React from 'react';
import { Center } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function centerComponent(props) {
  return React.createElement(Center, removeEmpty(props), props.children);
}
