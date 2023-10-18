import * as React from 'react';
import { Title } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function titleComponent(props) {
  return React.createElement(Title, removeEmpty(props), props.children);
}
