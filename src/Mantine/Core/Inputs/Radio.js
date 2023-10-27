import * as React from 'react';
import { Radio } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function radioComponent(props) {
  return React.createElement(Radio, removeEmpty(props), props.children);
}

export function radioGroupComponent(props) {
  return React.createElement(Radio.Group, removeEmpty(props), props.children);
}
