import * as React from 'react';
import { NumberFormatter } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function numberFormatterComponent(props) {
  return React.createElement(NumberFormatter, removeEmpty(props), props.children);
}
