import * as React from 'react';
import { Collapse } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function collapseComponent(props) {
  return React.createElement(Collapse, removeEmpty(props), props.children);
}
