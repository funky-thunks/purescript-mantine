import * as React from 'react';
import { Fieldset } from '@mantine/core';

export function fieldsetComponent(props) {
  return React.createElement(Fieldset, props, props.children);
}
