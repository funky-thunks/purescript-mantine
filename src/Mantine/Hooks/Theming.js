import { useMantineColorScheme } from '@mantine/core';

export function useMantineColorScheme_(options, tuple) {
  const { colorScheme, toggleColorScheme } = useMantineColorScheme(options);
  return tuple(colorScheme, toggleColorScheme);
}
