import * as React from 'react';
import { MediaQuery } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function mediaQueryComponent(props) {
  return React.createElement(MediaQuery, removeEmpty(props), props.children);
}
