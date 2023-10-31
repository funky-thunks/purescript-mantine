import * as React from 'react';
import { AppShell, Navbar, Header, Aside, Footer } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function appShellComponent(props) {
  return React.createElement(AppShell, removeEmpty(props), props.children);
}

export function appShellMainComponent(props) {
  return React.createElement(AppShell.Main, removeEmpty(props), props.children);
}

export function appShellNavbarComponent(props) {
  return React.createElement(AppShell.Navbar, removeEmpty(props), props.children);
}

export function appShellSectionComponent(props) {
  return React.createElement(AppShell.Section, removeEmpty(props), props.children);
}

export function appShellHeaderComponent(props) {
  return React.createElement(AppShell.Header, removeEmpty(props), props.children);
}

export function appShellAsideComponent(props) {
  return React.createElement(AppShell.Aside, removeEmpty(props), props.children);
}

export function appShellFooterComponent(props) {
  return React.createElement(AppShell.Footer, removeEmpty(props), props.children);
}
