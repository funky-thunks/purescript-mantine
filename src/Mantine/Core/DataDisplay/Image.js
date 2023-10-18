import * as React from 'react';
import { Image } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function imageComponent(props) {
  return React.createElement(Image, removeEmpty(props), props.children);
}
