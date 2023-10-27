import * as React from 'react';
import { Rating } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function ratingComponent(props) {
  return React.createElement(Rating, removeEmpty(props), props.children);
}
