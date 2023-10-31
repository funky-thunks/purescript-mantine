import * as React from 'react';
import { Accordion } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function accordionComponent(props) {
  props.multiple = false;
  return React.createElement(Accordion, removeEmpty(props), props.children);
}

export function multipleAccordionComponent(props) {
  props.multiple = true;
  return React.createElement(Accordion, removeEmpty(props), props.children);
}

export function accordionControlComponent(props) {
  return React.createElement(Accordion.Control, removeEmpty(props), props.children);
}

export function accordionItemComponent(props) {
  return React.createElement(Accordion.Item, removeEmpty(props), props.children);
}

export function accordionPanelComponent(props) {
  return React.createElement(Accordion.Panel, removeEmpty(props), props.children);
}
