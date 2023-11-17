import * as React from 'react';
import { Burger } from '@mantine/core';

export function burgerComponent(props) {
  return React.createElement(Burger, props, props.children);
}
