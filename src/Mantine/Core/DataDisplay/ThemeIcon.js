import * as React from 'react';
import { ThemeIcon } from '@mantine/core';

export function themeIconComponent(props) {
  return React.createElement(ThemeIcon, props, props.children);
}
