import * as React from 'react';
import { ColorPicker } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function colorPickerComponent(props) {
  return React.createElement(ColorPicker, removeEmpty(props), null);
}
