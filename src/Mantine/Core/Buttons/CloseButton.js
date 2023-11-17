import * as React from 'react';
import { CloseButton } from '@mantine/core';

export function closeButtonComponent(props) {
  return React.createElement(CloseButton, props, props.children);
}
