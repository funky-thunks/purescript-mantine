import * as React from 'react';
import { Loader } from '@mantine/core';

export function loaderComponent(props) {
  return React.createElement(Loader, props, props.children);
}
