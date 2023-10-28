import * as React from 'react';
import { ColorInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function colorInputComponent(props) {
  return React.createElement(ColorInput, removeEmpty(props), null);
}
