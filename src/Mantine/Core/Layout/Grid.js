import * as React from 'react';
import { Grid } from '@mantine/core';

export function gridComponent(props) {
  return React.createElement(Grid, props, props.children);
}

export function gridColComponent(props) {
  return React.createElement(Grid.Col, props, props.children);
}
