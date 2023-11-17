import * as React from 'react';
import { Input } from '@mantine/core';

export function inputComponent(props) {
  return React.createElement(Input, props, null);
}

export function inputWrapperComponent(props) {
  return React.createElement(Input.Wrapper, props, props.children);
}

export function inputLabelComponent(props) {
  return React.createElement(Input.Label, props, props.children);
}

export function inputDescriptionComponent(props) {
  return React.createElement(Input.Description, props, props.children);
}

export function inputErrorComponent(props) {
  return React.createElement(Input.Error, props, props.children);
}
