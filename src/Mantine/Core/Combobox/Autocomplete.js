import * as React from 'react';
import { Autocomplete } from '@mantine/core';

export function autocompleteComponent(props) {
  return React.createElement(Autocomplete, props, props.children);
}
