import * as React from 'react';
import { Skeleton } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function skeletonComponent(props) {
  return React.createElement(Skeleton, removeEmpty(props), props.children);
}
