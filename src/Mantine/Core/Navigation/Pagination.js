import * as React from 'react';
import { Pagination } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function paginationComponent(props) {
  return React.createElement(Pagination, removeEmpty(props), props.children);
}
