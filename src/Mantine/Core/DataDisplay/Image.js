import * as React from 'react';
import { Image } from '@mantine/core';

export function imageComponent(props) {
  return React.createElement(Image, props, props.children);
}
