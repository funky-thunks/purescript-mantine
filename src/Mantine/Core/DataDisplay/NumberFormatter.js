import * as React from 'react';
import { NumberFormatter } from '@mantine/core';

export function numberFormatterComponent(props) {
  return React.createElement(NumberFormatter, props, props.children);
}
