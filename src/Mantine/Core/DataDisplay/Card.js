import * as React from 'react';
import { Card } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function cardComponent(props) {
  return React.createElement(Card, removeEmpty(props), props.children);
}

export function cardSectionComponent(props) {
  return React.createElement(Card.Section, removeEmpty(props), props.children);
}
