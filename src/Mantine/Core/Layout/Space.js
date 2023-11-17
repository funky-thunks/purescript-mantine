import * as React from 'react';
import { Space } from '@mantine/core';

export function spaceComponent(props) {
  return React.createElement(Space, props, props.children);
}
