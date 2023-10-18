import * as React from 'react';
import { Indicator } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function indicatorComponent(props) {
  return React.createElement(Indicator, removeEmpty(props), props.children);
}
