import * as React from 'react';
import { Code } from '@mantine/core';

export function codeComponent(props) {
  return React.createElement(Code, props, props.children);
}
