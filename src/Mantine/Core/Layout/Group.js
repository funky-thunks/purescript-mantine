import * as React from 'react';
import { Group } from '@mantine/core';

export function groupComponent(props) {
  return React.createElement(Group, props, props.children);
}
