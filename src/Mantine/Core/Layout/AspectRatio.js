import * as React from 'react';
import { AspectRatio } from '@mantine/core';

export function aspectRatioComponent(props) {
  return React.createElement(AspectRatio, props, props.children);
}
