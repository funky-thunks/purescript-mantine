import * as React from 'react';
import { BackgroundImage } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function backgroundImageComponent(props) {
  return React.createElement(BackgroundImage, removeEmpty(props), props.children);
}
