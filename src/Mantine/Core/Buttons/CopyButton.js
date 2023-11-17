import * as React from 'react';
import { CopyButton } from '@mantine/core';

export function copyButtonComponent(props) {
  return React.createElement(CopyButton, props, props.children);
}
