import * as React from 'react';
import { Avatar } from '@mantine/core';

export function avatarComponent(props) {
  return React.createElement(Avatar, props, props.children);
}

export function avatarGroupComponent(props) {
  return React.createElement(Avatar.Group, props, props.children);
}
