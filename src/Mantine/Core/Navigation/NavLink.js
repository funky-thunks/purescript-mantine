import * as React from 'react';
import { NavLink } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function navLinkComponent(props) {
  return React.createElement(NavLink, removeEmpty(props), props.children);
}
