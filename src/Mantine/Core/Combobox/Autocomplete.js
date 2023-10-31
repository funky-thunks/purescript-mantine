import * as React from 'react';
import { Autocomplete } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function autocompleteComponent(props) {
  return React.createElement(Autocomplete, removeEmpty(props), props.children);
}
