import * as React from 'react';
import { SimpleGrid } from '@mantine/core';

export function simpleGridComponent(props) {
  return React.createElement(SimpleGrid, props, props.children);
}
