import * as React from 'react';
import { TextInput } from '@mantine/core';

export function textInputComponent(props) {
  return React.createElement(TextInput, props, props.children);
}
