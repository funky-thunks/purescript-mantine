import * as React from 'react';
import { Text } from '@mantine/core';

export function textComponent(props) {
  return React.createElement(Text, props, props.children);
}
