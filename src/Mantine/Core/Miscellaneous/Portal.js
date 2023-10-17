import * as React from 'react';
import { Portal } from '@mantine/core';

export function portalComponent(props) {
  return React.createElement(Portal, props, props.children);
}
