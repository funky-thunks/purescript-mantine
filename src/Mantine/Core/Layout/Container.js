import * as React from 'react';
import { Container } from '@mantine/core';

export function containerComponent(props) {
  return React.createElement(Container, props, props.children);
}
