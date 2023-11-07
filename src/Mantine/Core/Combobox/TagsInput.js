import * as React from 'react';
import { TagsInput } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function tagsInputComponent(props) {
  return React.createElement(TagsInput, removeEmpty(props), props.children);
}
