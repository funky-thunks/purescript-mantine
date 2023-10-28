import * as React from 'react';
import { FileInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function fileInputComponent(props) {
  return React.createElement(FileInput, removeEmpty(props), null);
}
