import * as React from 'react';
import { ActionIcon } from '@mantine/core';
import { removeEmpty } from '../../src/utils.js';

export function actionIconComponent(props) {
  return React.createElement(ActionIcon, removeEmpty(props), props.children);
}
