import * as React from 'react';
import { PinInput } from '@mantine/core';

export function pinInputComponent(props) {
  return React.createElement(PinInput, props, props.children);
}
