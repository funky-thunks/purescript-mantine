import * as React from 'react';
import { MultiSelect, Select } from '@mantine/core';

export function multiSelectComponent(props) {
  return React.createElement(MultiSelect, props, props.children);
}

export function selectComponent(props) {
  return React.createElement(Select, props, props.children);
}
