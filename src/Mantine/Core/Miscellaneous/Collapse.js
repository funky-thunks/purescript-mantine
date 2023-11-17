import * as React from 'react';
import { Collapse } from '@mantine/core';

export function collapseComponent(props) {
  return React.createElement(Collapse, props, props.children);
}
