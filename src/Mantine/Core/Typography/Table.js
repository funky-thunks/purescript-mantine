import * as React from 'react';
import { Table } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function tableComponent(props) {
  return React.createElement(Table, removeEmpty(props), props.children);
}
