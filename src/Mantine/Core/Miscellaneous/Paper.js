import * as React from 'react';
import { Paper } from '@mantine/core';

export function paperComponent(props) {
  return React.createElement(Paper, props, props.children);
}
