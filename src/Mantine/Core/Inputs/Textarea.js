import * as React from 'react';
import { Textarea } from '@mantine/core';

export function textareaComponent(props) {
  return React.createElement(Textarea, props, props.children);
}
