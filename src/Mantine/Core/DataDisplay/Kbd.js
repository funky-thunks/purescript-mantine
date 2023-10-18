import * as React from 'react';
import { Kbd } from '@mantine/core';

export function kbdComponent(props) {
  return React.createElement(Kbd, props, props.children);
}
