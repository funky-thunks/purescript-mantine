import * as React from 'react';
import { RingProgress } from '@mantine/core';

export function ringProgressComponent(props) {
  return React.createElement(RingProgress, props, props.children);
}
