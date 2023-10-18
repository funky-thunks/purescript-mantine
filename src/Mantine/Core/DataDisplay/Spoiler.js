import * as React from 'react';
import { Spoiler } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function spoilerComponent(props) {
  return React.createElement(Spoiler, removeEmpty(props), props.children);
}
