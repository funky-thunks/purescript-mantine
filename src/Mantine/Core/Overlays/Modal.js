import * as React from 'react';
import { Drawer, Modal } from '@mantine/core';

export function drawerComponent(props) {
  return React.createElement(Drawer, props, props.children);
}

export function modalComponent(props) {
  return React.createElement(Modal, props, props.children);
}
