import * as React from 'react';
import { TagsInput } from '@mantine/core';

export function tagsInputComponent(props) {
  return React.createElement(TagsInput, props, props.children);
}
