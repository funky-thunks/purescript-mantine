import * as React from 'react';
import { Alert } from '@mantine/core';

export function alertComponent(props) {
  return React.createElement(Alert, props, props.children);
}
