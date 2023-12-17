import * as React from 'react';
import { Combobox, useCombobox } from '@mantine/core';

function applyStore(store, options) {
  return
    Object.assign({}, opts,
      {
        onOpenedChange:  opts.onOpenedChange  === undefined ? undefined : arg => opts.onOpenedChange( store, arg),
        onDropdownClose: opts.onDropdownClose === undefined ? undefined : arg => opts.onDropdownClose(store, arg),
        onDropdownOpen:  opts.onDropdownOpen  === undefined ? undefined : arg => opts.onDropdownOpen( store, arg),
      }
    );
}

export function useComboboxImpl(options) {
  return () => {
    const store = useCombobox(applyStore(store, options));
    return store;
  };
}

export function comboboxComponent(props) {
  return React.createElement(Combobox, props, props.children);
}

export function comboboxOptionsComponent(props) {
  return React.createElement(Combobox.Options, props, props.children);
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

export function comboboxSearchComponent(props) {
  return React.createElement(Combobox.Search, props, props.children);
}
