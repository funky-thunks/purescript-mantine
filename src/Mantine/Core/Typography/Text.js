import * as React from 'react';
import { Text } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function textComponent(props) {
  return React.createElement(Text, removeEmpty(props), props.children);
}
