import * as React from 'react';
import { FileButton } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function fileButtonComponent(props) {
  props.multiple = false;
  return React.createElement(FileButton, removeEmpty(props), props.children);
}

export function multipleFileButtonComponent(props) {
  props.multiple = true;
  return React.createElement(FileButton, removeEmpty(props), props.children);
}
