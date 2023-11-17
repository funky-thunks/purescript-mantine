import * as React from 'react';
import { Divider } from '@mantine/core';

export function dividerComponent(props) {
  return React.createElement(Divider, props, props.children);
}
