import * as React from 'react';
import { Highlight } from '@mantine/core';

export function highlightComponent(props) {
  return React.createElement(Highlight, props, props.children);
}
