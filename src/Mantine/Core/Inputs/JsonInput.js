import * as React from 'react';
import { JsonInput } from '@mantine/core';

export function jsonInputComponent(props) {
  return React.createElement(JsonInput, props, props.children);
}
