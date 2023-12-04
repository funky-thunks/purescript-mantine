import * as React from 'react';
import { Progress } from '@mantine/core';

export function progressComponent(props) {
  return React.createElement(Progress, props, props.children);
}

export function progressRootComponent(props) {
  return React.createElement(Progress.Root, props, props.children);
}

export function progressSectionComponent(props) {
  return React.createElement(Progress.Section, props, props.children);
}

export function progressLabelComponent(props) {
  return React.createElement(Progress.Label, props, props.children);
}
