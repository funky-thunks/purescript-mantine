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

export function navbarComponent(props) {
  return React.createElement(Navbar, removeEmpty(props), props.children);
}

export function navbarSectionComponent(props) {
  return React.createElement(Navbar.Section, removeEmpty(props), props.children);
}

export function headerComponent(props) {
  return React.createElement(Header, removeEmpty(props), props.children);
}

export function asideComponent(props) {
  return React.createElement(Aside, removeEmpty(props), props.children);
}

export function footerComponent(props) {
  return React.createElement(Footer, removeEmpty(props), props.children);
}
