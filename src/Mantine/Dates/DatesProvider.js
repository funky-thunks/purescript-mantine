import * as React from 'react';
import { DatesProvider } from '@mantine/dates';

export function datesProviderComponent(props) {
  return React.createElement(DatesProvider, props, props.children);
}
