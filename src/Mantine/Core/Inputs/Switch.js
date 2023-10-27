import * as React from 'react';
import { Switch } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function switchComponent(props) {
  return React.createElement(Switch, removeEmpty(props), props.children);
}

export function switchGroupComponent(props) {
  return React.createElement(Switch.Group, removeEmpty(props), props.children);
}
