import * as React from 'react';
import { TypographyStylesProvider } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function typographyStylesProviderComponent(props) {
  return React.createElement(TypographyStylesProvider, removeEmpty(props), props.children);
}
