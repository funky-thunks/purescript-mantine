import * as React from 'react';
import { Title } from '@mantine/core';

export function titleComponent(props) {
  return React.createElement(Title, props, props.children);
}
