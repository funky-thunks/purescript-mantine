import * as React from 'react';
import { Flex } from '@mantine/core';

export function flexComponent(props) {
  return React.createElement(Flex, props, props.children);
}
