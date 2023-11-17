import * as React from 'react';
import { TypographyStylesProvider } from '@mantine/core';

export function typographyStylesProviderComponent(props) {
  return React.createElement(TypographyStylesProvider, props, props.children);
}
