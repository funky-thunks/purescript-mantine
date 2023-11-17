import * as React from 'react';
import { BackgroundImage } from '@mantine/core';

export function backgroundImageComponent(props) {
  return React.createElement(BackgroundImage, props, props.children);
}
