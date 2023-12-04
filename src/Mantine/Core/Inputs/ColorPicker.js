import * as React from 'react';
import { AlphaSlider, ColorPicker, HueSlider } from '@mantine/core';

export function colorPickerComponent(props) {
  return React.createElement(ColorPicker, props, null);
}

export function alphaSliderComponent(props) {
  return React.createElement(AlphaSlider, props, null);
}

export function hueSliderComponent(props) {
  return React.createElement(HueSlider, props, null);
}
