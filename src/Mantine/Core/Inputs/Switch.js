import * as React from 'react';
import { Switch } from '@mantine/core';

export function switchComponent(props) {
  return React.createElement(Switch, props, props.children);
}

export function switchGroupComponent(props) {
  return React.createElement(Switch.Group, props, props.children);
}
