import * as React from 'react';
import { Center } from '@mantine/core';

export function centerComponent(props) {
  return React.createElement(Center, props, props.children);
}
