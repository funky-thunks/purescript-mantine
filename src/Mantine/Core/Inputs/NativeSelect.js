import * as React from 'react';
import { NativeSelect } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function nativeSelectComponent(props) {
  return React.createElement(NativeSelect, removeEmpty(props), props.children);
}
