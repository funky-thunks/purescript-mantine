import * as React from 'react';
import { HoverCard, Popover } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function hoverCardComponent(props) {
  return React.createElement(HoverCard, removeEmpty(props), props.children);
}

export function hoverCardTargetComponent(props) {
  return React.createElement(HoverCard.Target, removeEmpty(props), props.children);
}

export function hoverCardDropdownComponent(props) {
  return React.createElement(HoverCard.Dropdown, removeEmpty(props), props.children);
}

export function popoverComponent(props) {
  return React.createElement(Popover, removeEmpty(props), props.children);
}

export function popoverTargetComponent(props) {
  return React.createElement(Popover.Target, removeEmpty(props), props.children);
}

export function popoverDropdownComponent(props) {
  return React.createElement(Popover.Dropdown, removeEmpty(props), props.children);
}
