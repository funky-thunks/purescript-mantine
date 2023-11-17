import * as React from 'react';
import { Stepper } from '@mantine/core';

export function stepperComponent(props) {
  return React.createElement(Stepper, props, props.children);
}

export function stepperStepComponent(props) {
  return React.createElement(Stepper.Step, props, props.children);
}

export function stepperCompletedComponent(props) {
  return React.createElement(Stepper.Completed, props, props.children);
}

function StepFragmentComponent(f)(props) {
  return (
    <Fragment>{f(props.step)}</Fragment>
  );
}

export const stepFragmentComponent = StepFragmentComponent;
