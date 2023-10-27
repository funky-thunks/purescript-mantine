import * as React from 'react';
import { JsonInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function jsonInputComponent(props) {
  return React.createElement(JsonInput, removeEmpty(props), props.children);
}
