import * as React from 'react';
import { Transition } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function transitionComponent(props) {
  return React.createElement(Transition, removeEmpty(props), props.children);
}
