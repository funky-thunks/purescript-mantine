import * as React from 'react';
import { SegmentedControl } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function segmentedControlComponent(props) {
  return React.createElement(SegmentedControl, removeEmpty(props), props.children);
}
