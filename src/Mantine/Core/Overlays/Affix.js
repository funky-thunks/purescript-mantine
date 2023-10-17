import * as React from 'react';
import { Affix } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function affixComponent(props) {
  return React.createElement(Affix, removeEmpty(props), props.children);
}
