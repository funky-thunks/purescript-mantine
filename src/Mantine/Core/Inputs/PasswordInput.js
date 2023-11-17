import * as React from 'react';
import { PasswordInput } from '@mantine/core';

export function passwordInputComponent(props) {
  return React.createElement(PasswordInput, props, props.children);
}
