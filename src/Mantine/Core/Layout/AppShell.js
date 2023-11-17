import * as React from 'react';
import { AppShell, Navbar, Header, Aside, Footer, ScrollArea } from '@mantine/core';

export function appShellComponent(props) {
  return React.createElement(AppShell, props, props.children);
}

export function appShellMainComponent(props) {
  return React.createElement(AppShell.Main, props, props.children);
}

export function appShellNavbarComponent(props) {
  return React.createElement(AppShell.Navbar, props, props.children);
}

export function appShellSectionComponent(props) {
  return React.createElement(AppShell.Section, props, props.children);
}

export function appShellScrollableSectionComponent(props) {
  const props_ = Object.assign({}, props, { component: ScrollArea });
  return React.createElement(AppShell.Section, props_, props_.children);
}

export function appShellHeaderComponent(props) {
  return React.createElement(AppShell.Header, props, props.children);
}

export function appShellAsideComponent(props) {
  return React.createElement(AppShell.Aside, props, props.children);
}

export function appShellFooterComponent(props) {
  return React.createElement(AppShell.Footer, props, props.children);
}
