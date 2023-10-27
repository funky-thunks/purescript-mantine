import * as React from 'react';
import { PasswordInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function passwordInputComponent(props) {
  return React.createElement(PasswordInput, removeEmpty(props), props.children);
}
