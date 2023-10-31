import * as React from 'react';
import { PinInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function pinInputComponent(props) {
  return React.createElement(PinInput, removeEmpty(props), props.children);
}
