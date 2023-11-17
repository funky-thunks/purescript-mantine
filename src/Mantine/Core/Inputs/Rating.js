import * as React from 'react';
import { Rating } from '@mantine/core';

export function ratingComponent(props) {
  return React.createElement(Rating, props, props.children);
}
