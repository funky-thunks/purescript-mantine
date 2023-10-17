import * as React from 'react';
import { Tabs } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function tabsComponent(props) {
  return React.createElement(Tabs, removeEmpty(props), props.children);
}

export function tabComponent(props) {
  return React.createElement(Tabs.Tab, removeEmpty(props), props.children);
}

export function tabListComponent(props) {
  return React.createElement(Tabs.List, removeEmpty(props), props.children);
}

export function tabPanelComponent(props) {
  return React.createElement(Tabs.Panel, removeEmpty(props), props.children);
}
