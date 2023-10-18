import * as React from 'react';
import { Button, UnstyledButton } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export const buttonComponent = Button;

export function buttonGroupComponent(props) {
  return React.createElement(Button.Group, props, props.children)
}

export function unstyledButtonComponent(props) {
  return React.createElement(UnstyledButton, removeEmpty(props), props.children);
}
