import * as React from 'react';
import { ColorSwatch } from '@mantine/core';

export function colorSwatchComponent(props) {
  return React.createElement(ColorSwatch, props, props.children);
}
