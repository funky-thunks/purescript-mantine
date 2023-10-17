import * as React from 'react';
import { Box } from '@mantine/core';

export function boxComponent(props) {
  return React.createElement(Box, props, props.children);
}
