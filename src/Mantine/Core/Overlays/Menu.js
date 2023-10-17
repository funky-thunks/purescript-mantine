import * as React from 'react';
import { Menu } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function menuComponent(props) {
  return React.createElement(Menu, removeEmpty(props), props.children);
}

export function menuItemComponent(props) {
  return React.createElement(Menu.Item, removeEmpty(props), props.children);
}

export function menuDropdownComponent(props) {
  return React.createElement(Menu.Dropdown, removeEmpty(props), props.children);
}

export function menuTargetComponent(props) {
  return React.createElement(Menu.Target, removeEmpty(props), props.children);
}

export function menuLabelComponent(props) {
  return React.createElement(Menu.Label, removeEmpty(props), props.children);
}

export function menuDividerComponent(props) {
  return React.createElement(Menu.Divider, removeEmpty(props), props.children);
}
