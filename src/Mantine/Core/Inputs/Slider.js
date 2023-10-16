import * as React from 'react';
import { Slider, RangeSlider } from '@mantine/core';

function removeEmpty(obj) {
  return Object.fromEntries(
    Object.entries(obj)
      .filter(([_, v]) => typeof v === 'function' || v !== null)
      .map(([k, v]) => [k, v])
  );
}

export function sliderComponent(props) {
  return React.createElement(Slider, removeEmpty(props), props.children);
}

export function rangeSliderComponent(props) {
  return React.createElement(RangeSlider, removeEmpty(props), props.children);
}
