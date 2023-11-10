import * as React from 'react';
import { Calendar, DateInput, DatePicker, DatePickerInput, DateTimePicker, TimeInput } from '@mantine/dates';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function calendarComponent(props) {
  return React.createElement(Calendar, removeEmpty(props), props.children);
}

export function dateInputComponent(props) {
  return React.createElement(DateInput, removeEmpty(props), props.children);
}

export function datePickerComponent(props) {
  return React.createElement(DatePicker, removeEmpty(props), props.children);
}

export function datePickerInputComponent(props) {
  return React.createElement(DatePickerInput, removeEmpty(props), props.children);
}

export function dateTimePickerComponent(props) {
  return React.createElement(DateTimePicker, removeEmpty(props), props.children);
}

export function timeInputComponent(props) {
  return React.createElement(TimeInput, removeEmpty(props), props.children);
}
