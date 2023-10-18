import * as React from 'react';
import { List } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function listComponent(props) {
  return React.createElement(List, removeEmpty(props), props.children);
}

export function listItemComponent(props) {
  return React.createElement(List.Item, removeEmpty(props), props.children);
}
