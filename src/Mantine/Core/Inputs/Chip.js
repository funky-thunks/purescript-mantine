import * as React from 'react';
import { Chip } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function chipComponent(props) {
  return React.createElement(Chip, removeEmpty(props), props.children);
}

export function chipGroupComponent(props) {
  props.multiple = false;
  return React.createElement(Chip.Group, removeEmpty(props), props.children);
}

export function multipleChipGroupComponent(props) {
  props.multiple = true;
  return React.createElement(Chip.Group, removeEmpty(props), props.children);
}
