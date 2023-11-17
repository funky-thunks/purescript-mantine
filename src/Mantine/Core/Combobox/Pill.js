import * as React from 'react';
import { Pill } from '@mantine/core';

export function pillComponent(props) {
  return React.createElement(Pill, props, props.children);
}

export function pillGroupComponent(props) {
  return React.createElement(Pill.Group, props, props.children);
}
