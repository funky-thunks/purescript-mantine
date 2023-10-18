import * as React from 'react';
import { FocusTrap } from '@mantine/core';

export function focusTrapComponent(props) {
  return React.createElement(FocusTrap, props, props.children);
}
