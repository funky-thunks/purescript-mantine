import * as React from 'react';
import { Badge } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function badgeComponent(props) {
  return React.createElement(Badge, removeEmpty(props), props.children);
}
