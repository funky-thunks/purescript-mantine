import * as React from 'react';
import { Calendar, DateInput, DatePicker, DatePickerInput, DateTimePicker, TimeInput } from '@mantine/dates';

export function calendarComponent(props) {
  return React.createElement(Calendar, props, props.children);
}

export function dateInputComponent(props) {
  return React.createElement(DateInput, props, props.children);
}

export function datePickerComponent(props) {
  return React.createElement(DatePicker, props, props.children);
}

export function datePickerInputComponent(props) {
  return React.createElement(DatePickerInput, props, props.children);
}

export function dateTimePickerComponent(props) {
  return React.createElement(DateTimePicker, props, props.children);
}

export function timeInputComponent(props) {
  return React.createElement(TimeInput, props, props.children);
}
