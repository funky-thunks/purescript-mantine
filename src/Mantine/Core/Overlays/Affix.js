import * as React from 'react';
import { Affix } from '@mantine/core';

export function affixComponent(props) {
  return React.createElement(Affix, props, props.children);
}
