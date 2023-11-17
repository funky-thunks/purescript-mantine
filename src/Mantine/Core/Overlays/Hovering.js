import * as React from 'react';
import { HoverCard, Popover } from '@mantine/core';

export function hoverCardComponent(props) {
  return React.createElement(HoverCard, props, props.children);
}

export function hoverCardTargetComponent(props) {
  return React.createElement(HoverCard.Target, props, props.children);
}

export function hoverCardDropdownComponent(props) {
  return React.createElement(HoverCard.Dropdown, props, props.children);
}

export function popoverComponent(props) {
  return React.createElement(Popover, props, props.children);
}

export function popoverTargetComponent(props) {
  return React.createElement(Popover.Target, props, props.children);
}

export function popoverDropdownComponent(props) {
  return React.createElement(Popover.Dropdown, props, props.children);
}
