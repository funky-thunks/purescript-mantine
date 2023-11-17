import * as React from 'react';
import { Stack } from '@mantine/core';

export function stackComponent(props) {
  return React.createElement(Stack, props, props.children);
}
