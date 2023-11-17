import * as React from 'react';
import { Mark } from '@mantine/core';

export function markComponent(props) {
  return React.createElement(Mark, props, props.children);
}
