import * as React from 'react';
import { MantineProvider } from '@mantine/core';

import '@mantine/core/styles.css';

export function mantineProviderComponent(props) {
  return (
    <MantineProvider withGlobalStyles withNormalizeCSS>
      {props.children}
    </MantineProvider>
  );
}
