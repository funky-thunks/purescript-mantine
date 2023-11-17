import * as React from 'react';
import { Button, UnstyledButton } from '@mantine/core';

export function buttonComponent(props) {
  return React.createElement(Button, props, props.children);
}

export function buttonGroupComponent(props) {
  return React.createElement(Button.Group, props, props.children);
}

export function unstyledButtonComponent(props) {
  return React.createElement(UnstyledButton, props, props.children);
}
