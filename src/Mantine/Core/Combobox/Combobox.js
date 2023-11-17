import * as React from 'react';
import { Combobox } from '@mantine/core';

export function comboboxComponent(props) {
  return React.createElement(Combobox, props, props.children);
}

export function comboboxOptionComponent(props) {
  return React.createElement(Combobox.Option, props, props.children);
}

export function comboboxTargetComponent(props) {
  return React.createElement(Combobox.Target, props, props.children);
}

export function comboboxDropdownTargetComponent(props) {
  return React.createElement(Combobox.DropdownTarget, props, props.children);
}

export function comboboxEventsTargetComponent(props) {
  return React.createElement(Combobox.EventsTarget, props, props.children);
}

export function comboboxDropdownComponent(props) {
  return React.createElement(Combobox.Dropdown, props, props.children);
}

export function comboboxGroupComponent(props) {
  return React.createElement(Combobox.Group, props, props.children);
}
