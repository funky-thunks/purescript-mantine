import * as React from 'react';
import { Breadcrumbs } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function breadcrumbsComponent(props) {
  return React.createElement(Breadcrumbs, removeEmpty(props), props.children);
}
