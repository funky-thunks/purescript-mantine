import * as React from 'react';
import { Textarea } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function textareaComponent(props) {
  return React.createElement(Textarea, removeEmpty(props), props.children);
}
