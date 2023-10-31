import * as React from 'react';
import { Table } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function tableComponent(props) {
  return React.createElement(Table, removeEmpty(props), props.children);
}

export function tableTdComponent(props) {
  return React.createElement(Table.Td, removeEmpty(props), props.children);
}

export function tableThComponent(props) {
  return React.createElement(Table.Th, removeEmpty(props), props.children);
}

export function tableTrComponent(props) {
  return React.createElement(Table.Tr, removeEmpty(props), props.children);
}

export function tableTheadComponent(props) {
  return React.createElement(Table.Thead, removeEmpty(props), props.children);
}

export function tableTbodyComponent(props) {
  return React.createElement(Table.Tbody, removeEmpty(props), props.children);
}

export function tableTfootComponent(props) {
  return React.createElement(Table.Tfoot, removeEmpty(props), props.children);
}

export function tableCaptionComponent(props) {
  return React.createElement(Table.Caption, removeEmpty(props), props.children);
}

export function tableScrollContainerComponent(props) {
  return React.createElement(Table.ScrollContainer, removeEmpty(props), props.children);
}
