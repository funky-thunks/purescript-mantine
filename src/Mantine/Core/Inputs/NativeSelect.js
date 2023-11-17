import * as React from 'react';
import { NativeSelect } from '@mantine/core';

export function nativeSelectComponent(props) {
  return React.createElement(NativeSelect, props, props.children);
}
