import * as React from 'react';
import { Skeleton } from '@mantine/core';

export function skeletonComponent(props) {
  return React.createElement(Skeleton, props, props.children);
}
