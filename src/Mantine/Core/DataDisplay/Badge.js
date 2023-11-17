import * as React from 'react';
import { Badge } from '@mantine/core';

export function badgeComponent(props) {
  return React.createElement(Badge, props, props.children);
}
