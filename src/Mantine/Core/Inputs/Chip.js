import * as React from 'react';
import { Chip } from '@mantine/core';

export function chipComponent(props) {
  return React.createElement(Chip, props, props.children);
}

export function chipGroupComponent(props) {
  const props_ = Object.assign({}, props, { multiple: false });
  return React.createElement(Chip.Group, props_, props_.children);
}

export function multipleChipGroupComponent(props) {
  const props_ = Object.assign({}, props, { multiple: true });
  return React.createElement(Chip.Group, props_, props_.children);
}
