import * as React from 'react';
import { Tabs } from '@mantine/core';

export function tabsComponent(props) {
  return React.createElement(Tabs, props, props.children);
}

export function tabComponent(props) {
  return React.createElement(Tabs.Tab, props, props.children);
}

export function tabListComponent(props) {
  return React.createElement(Tabs.List, props, props.children);
}

export function tabPanelComponent(props) {
  return React.createElement(Tabs.Panel, props, props.children);
}
