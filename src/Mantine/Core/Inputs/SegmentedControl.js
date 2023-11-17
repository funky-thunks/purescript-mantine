import * as React from 'react';
import { SegmentedControl } from '@mantine/core';

export function segmentedControlComponent(props) {
  return React.createElement(SegmentedControl, props, props.children);
}
