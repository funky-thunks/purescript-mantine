import * as React from 'react';
import { PillsInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function pillsInputComponent(props) {
  return React.createElement(PillsInput, removeEmpty(props), props.children);
}

export function pillsInputFieldComponent(props) {
  return React.createElement(PillsInput.Field, removeEmpty(props), props.children);
}
