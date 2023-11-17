import * as React from 'react';
import { Blockquote } from '@mantine/core';

export function blockquoteComponent(props) {
  return React.createElement(Blockquote, props, props.children);
}
