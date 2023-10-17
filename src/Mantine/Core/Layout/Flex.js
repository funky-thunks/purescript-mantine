import * as React from 'react';
import { Flex } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function flexComponent(props) {
  return React.createElement(Flex, removeEmpty(props), props.children);
}
