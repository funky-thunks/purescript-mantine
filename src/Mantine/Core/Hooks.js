import { useMantineColorScheme } from '@mantine/core';
import { useFocusWithin, useMove } from '@mantine/hooks';

export function useMantineColorScheme_(tuple) {
  const { colorScheme, toggleColorScheme } = useMantineColorScheme();
  return tuple(colorScheme, toggleColorScheme);
}

export const useMoveImpl = useMove;

export const useFocusWithinImpl = useFocusWithin;
