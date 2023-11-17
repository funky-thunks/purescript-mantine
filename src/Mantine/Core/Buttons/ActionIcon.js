import * as React from 'react';
import { ActionIcon } from '@mantine/core';

export function actionIconComponent(props) {
  return React.createElement(ActionIcon, props, props.children);
}

export function actionIconGroupComponent(props) {
  return React.createElement(ActionIcon.Group, props, props.children);
}
