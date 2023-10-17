import * as React from 'react';
import { Container } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function containerComponent(props) {
  return React.createElement(Container, removeEmpty(props), props.children);
}
