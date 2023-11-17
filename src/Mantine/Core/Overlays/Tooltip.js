import * as React from 'react';
import { Tooltip } from '@mantine/core';

export function tooltipComponent(props) {
  return React.createElement(Tooltip, props, props.children);
}

export function tooltipFloatingComponent(props) {
  return React.createElement(Tooltip.Floating, props, props.children);
}

export function tooltipGroupComponent(props) {
  return React.createElement(Tooltip.Group, props, props.children);
}
