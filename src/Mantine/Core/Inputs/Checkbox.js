import * as React from 'react';
import { Checkbox } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function checkboxComponent(props) {
  return React.createElement(Checkbox, removeEmpty(props), props.children);
}
