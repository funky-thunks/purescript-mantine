import * as React from 'react';
import { Button, UnstyledButton } from '@mantine/core';
import { removeEmpty } from '../../src/utils.js';

export const buttonComponent = Button;

export function buttonGroupComponent(props) {
  return React.createElement(Button.Group, props, props.children)
}

export function unstyledButtonComponent(props) {
  return React.createElement(UnstyledButton, removeEmpty(props), props.children);
}
