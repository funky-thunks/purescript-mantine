import * as React from 'react';
import { Combobox } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function comboboxComponent(props) {
  return React.createElement(Combobox, removeEmpty(props), props.children);
}

export function comboboxOptionComponent(props) {
  return React.createElement(Combobox.Option, removeEmpty(props), props.children);
}

export function comboboxTargetComponent(props) {
  return React.createElement(Combobox.Target, removeEmpty(props), props.children);
}

export function comboboxDropdownTargetComponent(props) {
  return React.createElement(Combobox.DropdownTarget, removeEmpty(props), props.children);
}

export function comboboxEventsTargetComponent(props) {
  return React.createElement(Combobox.EventsTarget, removeEmpty(props), props.children);
}

export function comboboxDropdownComponent(props) {
  return React.createElement(Combobox.Dropdown, removeEmpty(props), props.children);
}

export function comboboxGroupComponent(props) {
  return React.createElement(Combobox.Group, removeEmpty(props), props.children);
}
