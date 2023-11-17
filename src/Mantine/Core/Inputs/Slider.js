import * as React from 'react';
import { Slider, RangeSlider } from '@mantine/core';

export function sliderComponent(props) {
  return React.createElement(Slider, props, props.children);
}

export function rangeSliderComponent(props) {
  return React.createElement(RangeSlider, props, props.children);
}
