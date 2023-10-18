import * as React from 'react';
import { ThemeIcon } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function themeIconComponent(props) {
  return React.createElement(ThemeIcon, removeEmpty(props), props.children);
}
