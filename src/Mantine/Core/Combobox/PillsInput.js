import * as React from 'react';
import { PillsInput } from '@mantine/core';

export function pillsInputComponent(props) {
  return React.createElement(PillsInput, props, props.children);
}

export function pillsInputFieldComponent(props) {
  return React.createElement(PillsInput.Field, props, props.children);
}
