import * as React from 'react';
import { Spoiler } from '@mantine/core';

export function spoilerComponent(props) {
  return React.createElement(Spoiler, props, props.children);
}
