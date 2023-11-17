import * as React from 'react';
import { Transition } from '@mantine/core';

export function transitionComponent(props) {
  return React.createElement(Transition, props, props.children);
}
