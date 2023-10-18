import * as React from 'react';
import { Code } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function codeComponent(props) {
  return React.createElement(Code, removeEmpty(props), props.children);
}
