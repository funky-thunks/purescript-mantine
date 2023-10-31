import * as React from 'react';
import { ActionIcon } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function actionIconComponent(props) {
  return React.createElement(ActionIcon, removeEmpty(props), props.children);
}

export function actionIconGroupComponent(props) {
  return React.createElement(ActionIcon.Group, removeEmpty(props), props.children);
}
