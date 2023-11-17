import * as React from 'react';
import { Pagination } from '@mantine/core';

export function paginationComponent(props) {
  return React.createElement(Pagination, props, props.children);
}
