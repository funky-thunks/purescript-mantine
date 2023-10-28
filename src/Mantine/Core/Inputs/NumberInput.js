import * as React from 'react';
import { NumberInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function numberInputComponent(props) {
  return React.createElement(NumberInput, removeEmpty(props), null);
}
