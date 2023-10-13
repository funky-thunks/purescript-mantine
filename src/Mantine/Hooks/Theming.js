import { useMantineColorScheme } from '@mantine/core';

export function useMantineColorScheme_(tuple) {
  const { colorScheme, toggleColorScheme } = useMantineColorScheme();
  return tuple(colorScheme, toggleColorScheme);
}
