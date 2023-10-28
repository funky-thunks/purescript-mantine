import * as React from 'react';
import { Input } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function inputComponent(props) {
  return React.createElement(Input, removeEmpty(props), null);
}

export function inputWrapperComponent(props) {
  return React.createElement(Input.Wrapper, removeEmpty(props), props.children);
}

export function inputLabelComponent(props) {
  return React.createElement(Input.Label, removeEmpty(props), props.children);
}

export function inputDescriptionComponent(props) {
  return React.createElement(Input.Description, removeEmpty(props), props.children);
}

export function inputErrorComponent(props) {
  return React.createElement(Input.Error, removeEmpty(props), props.children);
}
