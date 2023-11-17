import * as React from 'react';
import { List } from '@mantine/core';

export function listComponent(props) {
  return React.createElement(List, props, props.children);
}

export function listItemComponent(props) {
  return React.createElement(List.Item, props, props.children);
}
