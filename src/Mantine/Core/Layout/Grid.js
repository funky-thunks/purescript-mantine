import * as React from 'react';
import { Grid } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function gridComponent(props) {
  return React.createElement(Grid, removeEmpty(props), props.children);
}

export function gridColComponent(props) {
  return React.createElement(Grid.Col, removeEmpty(props), props.children);
}
