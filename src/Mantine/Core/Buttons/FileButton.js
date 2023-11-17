import * as React from 'react';
import { FileButton } from '@mantine/core';

export function fileButtonComponent(props) {
  const props_ = Object.assign({}, props, { multiple: false });
  return React.createElement(FileButton, props_, props_.children);
}

export function multipleFileButtonComponent(props) {
  const props_ = Object.assign({}, props, { multiple: true });
  return React.createElement(FileButton, props_, props_.children);
}
