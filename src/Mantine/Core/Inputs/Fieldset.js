import * as React from 'react';
import { Fieldset } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function fieldsetComponent(props) {
  return React.createElement(Fieldset, removeEmpty(props), props.children);
}
