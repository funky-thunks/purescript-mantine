import * as React from 'react';
import { ColorSwatch } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function colorSwatchComponent(props) {
  return React.createElement(ColorSwatch, removeEmpty(props), props.children);
}
