import * as React from 'react';
import { Group } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function groupComponent(props) {
  return React.createElement(Group, removeEmpty(props), props.children);
}
