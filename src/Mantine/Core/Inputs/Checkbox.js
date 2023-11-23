import * as React from 'react';
import { Checkbox, CheckIcon } from '@mantine/core';

export function checkboxComponent(props) {
  return React.createElement(Checkbox, props, props.children);
}

export function checkboxGroupComponent(props) {
  return React.createElement(Checkbox.Group, props, props.children);
}

export function checkIconComponent(props) {
  return React.createElement(CheckIcon, props, props.children);
}
