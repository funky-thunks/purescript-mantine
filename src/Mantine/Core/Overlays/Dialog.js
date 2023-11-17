import * as React from 'react';
import { Dialog } from '@mantine/core';

export function dialogComponent(props) {
  return React.createElement(Dialog, props, props.children);
}
