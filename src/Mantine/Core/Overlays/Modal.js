import * as React from 'react';
import { Modal } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function modalComponent(props) {
  return React.createElement(Modal, removeEmpty(props), props.children);
}
