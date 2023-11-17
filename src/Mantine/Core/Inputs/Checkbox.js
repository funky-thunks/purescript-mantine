import * as React from 'react';
import { Checkbox } from '@mantine/core';

export function checkboxComponent(props) {
  return React.createElement(Checkbox, props, props.children);
}

export function checkboxGroupComponent(props) {
  return React.createElement(Checkbox.Group, props, props.children);
}
