import * as React from 'react';
import { NavLink } from '@mantine/core';

export function navLinkComponent(props) {
  return React.createElement(NavLink, props, props.children);
}
