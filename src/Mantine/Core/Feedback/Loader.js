import * as React from 'react';
import { Loader } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function loaderComponent(props) {
  return React.createElement(Loader, removeEmpty(props), props.children);
}
