import * as React from 'react';
import { Burger } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function burgerComponent(props) {
  return React.createElement(Burger, removeEmpty(props), props.children);
}
