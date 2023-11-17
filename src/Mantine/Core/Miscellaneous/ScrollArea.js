import * as React from 'react';
import { ScrollArea } from '@mantine/core';

export function scrollAreaComponent(props) {
  return React.createElement(ScrollArea, props, props.children);
}

export function scrollAreaAutosizeComponent(props) {
  return React.createElement(ScrollArea.Autosize, props, props.children);
}
