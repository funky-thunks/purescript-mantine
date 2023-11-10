import * as React from 'react';
import { MonthPicker, MonthPickerInput, YearPicker, YearPickerInput } from '@mantine/dates';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function monthPickerComponent(props) {
  return React.createElement(MonthPicker, removeEmpty(props), props.children);
}

export function monthPickerInputComponent(props) {
  return React.createElement(MonthPickerInput, removeEmpty(props), props.children);
}

export function yearPickerComponent(props) {
  return React.createElement(YearPicker, removeEmpty(props), props.children);
}

export function yearPickerInputComponent(props) {
  return React.createElement(YearPickerInput, removeEmpty(props), props.children);
}
