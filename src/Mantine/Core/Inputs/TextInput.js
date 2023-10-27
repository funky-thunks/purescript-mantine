import * as React from 'react';
import { TextInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function textInputComponent(props) {
  return React.createElement(TextInput, removeEmpty(props), props.children);
}
