import * as React from 'react';
import { Breadcrumbs } from '@mantine/core';

export function breadcrumbsComponent(props) {
  return React.createElement(Breadcrumbs, props, props.children);
}
