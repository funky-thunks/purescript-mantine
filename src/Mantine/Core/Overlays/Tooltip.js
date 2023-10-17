import * as React from 'react';
import { Tooltip } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function tooltipComponent(props) {
  return React.createElement(Tooltip, removeEmpty(props), props.children);
}

export function tooltipFloatingComponent(props) {
  return React.createElement(Tooltip.Floating, removeEmpty(props), props.children);
}

export function tooltipGroupComponent(props) {
  return React.createElement(Tooltip.Group, removeEmpty(props), props.children);
}
