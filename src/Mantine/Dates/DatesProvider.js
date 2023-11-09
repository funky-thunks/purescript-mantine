import * as React from 'react';
import { DatesProvider } from '@mantine/dates';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function datesProviderComponent(props) {
  return React.createElement(DatesProvider, removeEmpty(props), props.children);
}
