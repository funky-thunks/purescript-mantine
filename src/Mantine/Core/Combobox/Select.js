import * as React from 'react';
import { MultiSelect, Select } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function multiSelectComponent(props) {
  return React.createElement(MultiSelect, removeEmpty(props), props.children);
}

export function selectComponent(props) {
  return React.createElement(Select, removeEmpty(props), props.children);
}