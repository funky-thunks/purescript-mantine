import * as React from 'react';
import { Table } from '@mantine/core';

export function tableComponent(props) {
  return React.createElement(Table, props, props.children);
}

export function tableTdComponent(props) {
  return React.createElement(Table.Td, props, props.children);
}

export function tableThComponent(props) {
  return React.createElement(Table.Th, props, props.children);
}

export function tableTrComponent(props) {
  return React.createElement(Table.Tr, props, props.children);
}

export function tableTheadComponent(props) {
  return React.createElement(Table.Thead, props, props.children);
}

export function tableTbodyComponent(props) {
  return React.createElement(Table.Tbody, props, props.children);
}

export function tableTfootComponent(props) {
  return React.createElement(Table.Tfoot, props, props.children);
}

export function tableCaptionComponent(props) {
  return React.createElement(Table.Caption, props, props.children);
}

export function tableScrollContainerComponent(props) {
  return React.createElement(Table.ScrollContainer, props, props.children);
}
