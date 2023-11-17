import * as React from 'react';
import { Accordion } from '@mantine/core';

export function accordionComponent(props) {
  const props_ = Object.assign({}, props, { multiple: false });
  return React.createElement(Accordion, props_, props_.children);
}

export function multipleAccordionComponent(props) {
  const props_ = Object.assign({}, props, { multiple: true });
  return React.createElement(Accordion, props_, props_.children);
}

export function accordionControlComponent(props) {
  return React.createElement(Accordion.Control, props, props.children);
}

export function accordionItemComponent(props) {
  return React.createElement(Accordion.Item, props, props.children);
}

export function accordionPanelComponent(props) {
  return React.createElement(Accordion.Panel, props, props.children);
}
