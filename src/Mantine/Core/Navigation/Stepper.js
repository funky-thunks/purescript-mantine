import * as React from 'react';
import { Stepper } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function stepperComponent(props) {
  return React.createElement(Stepper, removeEmpty(props), props.children);
}

export function stepperStepComponent(props) {
  return React.createElement(Stepper.Step, removeEmpty(props), props.children);
}

export function stepperCompletedComponent(props) {
  return React.createElement(Stepper.Completed, removeEmpty(props), props.children);
}

function StepFragmentComponent(f)(props) {
  return (
    <Fragment>{f(props.step)}</Fragment>
  );
}

export const stepFragmentComponent = StepFragmentComponent;
