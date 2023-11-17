import * as React from 'react';
import { Card } from '@mantine/core';

export function cardComponent(props) {
  return React.createElement(Card, props, props.children);
}

export function cardSectionComponent(props) {
  return React.createElement(Card.Section, props, props.children);
}
