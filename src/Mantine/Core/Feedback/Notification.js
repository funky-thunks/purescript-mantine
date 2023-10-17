import * as React from 'react';
import { Notification } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function notificationComponent(props) {
  return React.createElement(Notification, removeEmpty(props), props.children);
}
