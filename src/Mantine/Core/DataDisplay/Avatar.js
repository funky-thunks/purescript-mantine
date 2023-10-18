import * as React from 'react';
import { Avatar } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function avatarComponent(props) {
  return React.createElement(Avatar, removeEmpty(props), props.children);
}

export function avatarGroupComponent(props) {
  return React.createElement(Avatar.Group, removeEmpty(props), props.children);
}
