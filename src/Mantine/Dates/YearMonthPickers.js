import * as React from 'react';
import { MonthPicker, MonthPickerInput, YearPicker, YearPickerInput } from '@mantine/dates';

export function monthPickerComponent(props) {
  return React.createElement(MonthPicker, props, props.children);
}

export function monthPickerInputComponent(props) {
  return React.createElement(MonthPickerInput, props, props.children);
}

export function yearPickerComponent(props) {
  return React.createElement(YearPicker, props, props.children);
}

export function yearPickerInputComponent(props) {
  return React.createElement(YearPickerInput, props, props.children);
}
