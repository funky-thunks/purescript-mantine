import * as React from 'react';
import { VisuallyHidden } from '@mantine/core';

export function visuallyHiddenComponent(props) {
  return React.createElement(VisuallyHidden, props, props.children);
}
