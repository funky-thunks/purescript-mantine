import * as React from 'react';
import { Radio } from '@mantine/core';

export function radioComponent(props) {
  return React.createElement(Radio, props, props.children);
}

export function radioGroupComponent(props) {
  return React.createElement(Radio.Group, props, props.children);
}
