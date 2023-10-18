import * as React from 'react';
import { Timeline } from '@mantine/core';

export function timelineComponent(props) {
  return React.createElement(Timeline, props, props.children);
}

export function timelineItemComponent(props) {
  return React.createElement(Timeline.Item, props, props.children);
}
