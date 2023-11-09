import * as React from 'react';
import { Drawer, Modal } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function drawerComponent(props) {
  return React.createElement(Drawer, removeEmpty(props), props.children);
}

export function modalComponent(props) {
  return React.createElement(Modal, removeEmpty(props), props.children);
}
