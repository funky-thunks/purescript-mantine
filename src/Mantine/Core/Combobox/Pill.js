import * as React from 'react';
import { Pill } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function pillComponent(props) {
  return React.createElement(Pill, removeEmpty(props), props.children);
}

export function pillGroupComponent(props) {
  return React.createElement(Pill.Group, removeEmpty(props), props.children);
}
