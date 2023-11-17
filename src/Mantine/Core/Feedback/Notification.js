import * as React from 'react';
import { Notification } from '@mantine/core';

export function notificationComponent(props) {
  return React.createElement(Notification, props, props.children);
}
