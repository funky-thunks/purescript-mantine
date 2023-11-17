import * as React from 'react';
import { Indicator } from '@mantine/core';

export function indicatorComponent(props) {
  return React.createElement(Indicator, props, props.children);
}
