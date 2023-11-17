import * as React from 'react';
import { Menu } from '@mantine/core';

export function menuComponent(props) {
  return React.createElement(Menu, props, props.children);
}

export function menuItemComponent(props) {
  return React.createElement(Menu.Item, props, props.children);
}

export function menuDropdownComponent(props) {
  return React.createElement(Menu.Dropdown, props, props.children);
}

export function menuTargetComponent(props) {
  return React.createElement(Menu.Target, props, props.children);
}

export function menuLabelComponent(props) {
  return React.createElement(Menu.Label, props, props.children);
}

export function menuDividerComponent(props) {
  return React.createElement(Menu.Divider, props, props.children);
}
