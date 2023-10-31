import * as React from 'react';
import { OptionalPortal, Portal } from '@mantine/core';

export function optionalPortalComponent(props) {
  return React.createElement(OptionalPortal, props, props.children);
}

export function portalComponent(props) {
  return React.createElement(Portal, props, props.children);
}
