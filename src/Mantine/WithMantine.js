import * as React from 'react';
import { MantineProvider } from '@mantine/core';

export function mantineProviderComponent(props) {
  return (
    <MantineProvider withGlobalStyles withNormalizeCSS>
      {props.children}
    </MantineProvider>
  );
}
