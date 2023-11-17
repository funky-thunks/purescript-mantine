import * as React from 'react';
import { Anchor } from '@mantine/core';

export function anchorComponent(props) {
  return React.createElement(Anchor, props, props.children);
}
