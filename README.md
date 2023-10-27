# Purescript bindings for mantine

Bindings to use [Mantine](mantine.dev) components from a Purescript codebase.

Currently supporting mantine v5.

:warning: This is work-in-progress and this is very unstable.

## Progress

### Components

```purescript
import Mantine.Core
```

- [x] Layout
  - [x] [AppShell](https://v5.mantine.dev/core/app-shell/)
  - [x] [AspectRatio](https://v5.mantine.dev/core/aspect-ratio/)
  - [x] [Center](https://v5.mantine.dev/core/center/)
  - [x] [Container](https://v5.mantine.dev/core/container/)
  - [x] [Flex](https://v5.mantine.dev/core/flex/)
  - [x] [Grid](https://v5.mantine.dev/core/grid/)
  - [x] [Group](https://v5.mantine.dev/core/group/)
  - [x] [MediaQuery](https://v5.mantine.dev/core/media-query/)
  - [x] [SimpleGrid](https://v5.mantine.dev/core/simple-grid/)
  - [x] [Space](https://v5.mantine.dev/core/space/)
  - [x] [Stack](https://v5.mantine.dev/core/stack/)
- [ ] Buttons
  - [x] [ActionIcon](https://v5.mantine.dev/core/action-icon/)
  - [x] [Button](https://v5.mantine.dev/core/button/)
  - [x] [CloseButton](https://v5.mantine.dev/core/close-button/)
  - [x] [CopyButton](https://v5.mantine.dev/core/copy-button/)
  - [ ] [FileButton](https://v5.mantine.dev/core/file-button/)
  - [x] [UnstyledButton](https://v5.mantine.dev/core/unstyled-button)
- [ ] Inputs
  - [ ] [Autocomplete](https://v5.mantine.dev/core/autocomplete/)
  - [x] [Checkbox](https://v5.mantine.dev/core/checkbox/)
  - [x] [Chip](https://v5.mantine.dev/core/chip/)
  - [ ] [ColorInput](https://v5.mantine.dev/core/color-input/)
  - [ ] [ColorPicker](https://v5.mantine.dev/core/color-picker/)
  - [ ] [FileInput](https://v5.mantine.dev/core/file-input/)
  - [ ] [Input](https://v5.mantine.dev/core/input/)
  - [ ] [JsonInput](https://v5.mantine.dev/core/json-input/)
  - [ ] [MultiSelect](https://v5.mantine.dev/core/multi-select/)
  - [ ] [NativeSelect](https://v5.mantine.dev/core/native-select/)
  - [ ] [NumberInput](https://v5.mantine.dev/core/number-input/)
  - [ ] [PasswordInput](https://v5.mantine.dev/core/password-input/)
  - [ ] [Radio](https://v5.mantine.dev/core/radio/)
  - [ ] [Rating](https://v5.mantine.dev/core/rating/)
  - [ ] [SegmentedControl](https://v5.mantine.dev/core/segmented-control/)
  - [ ] [Select](https://v5.mantine.dev/core/select/)
  - [x] [Slider](https://v5.mantine.dev/core/slider/)
  - [ ] [Switch](https://v5.mantine.dev/core/switch/)
  - [ ] [Textarea](https://v5.mantine.dev/core/textarea/)
  - [ ] [TextInput](https://v5.mantine.dev/core/text-input/)
  - [ ] [TransferList](https://v5.mantine.dev/core/transfer-list/)
- [x] Navigation
  - [x] [Anchor](https://v5.mantine.dev/core/anchor/)
  - [x] [Breadcrumbs](https://v5.mantine.dev/core/breadcrumbs/)
  - [x] [Burger](https://v5.mantine.dev/core/burger/)
  - [x] [NavLink](https://v5.mantine.dev/core/nav-link/)
  - [x] [Pagination](https://v5.mantine.dev/core/pagination/)
  - [x] [Stepper](https://v5.mantine.dev/core/stepper/)
  - [x] [Tabs](https://v5.mantine.dev/core/tabs/)
- [x] Data display
  - [x] [Accordion](https://v5.mantine.dev/core/accordion/)
  - [x] [Avatar](https://v5.mantine.dev/core/avatar/)
  - [x] [BackgroundImage](https://v5.mantine.dev/core/background-image/)
  - [x] [Badge](https://v5.mantine.dev/core/badge/)
  - [x] [Card](https://v5.mantine.dev/core/card/)
  - [x] [ColorSwatch](https://v5.mantine.dev/core/color-swatch/)
  - [x] [Image](https://v5.mantine.dev/core/image/)
  - [x] [Indicator](https://v5.mantine.dev/core/indicator/)
  - [x] [Kbd](https://v5.mantine.dev/core/kbd/)
  - [x] [Spoiler](https://v5.mantine.dev/core/spoiler/)
  - [x] [ThemeIcon](https://v5.mantine.dev/core/theme-icon/)
  - [x] [Timeline](https://v5.mantine.dev/core/timeline/)
- [x] Overlays
  - [x] [Affix](https://v5.mantine.dev/core/affix/)
  - [x] [Dialog](https://v5.mantine.dev/core/dialog/)
  - [x] [Drawer](https://v5.mantine.dev/core/drawer/)
  - [x] [HoverCard](https://v5.mantine.dev/core/hover-card/)
  - [x] [LoadingOverlay](https://v5.mantine.dev/core/loading-overlay/)
  - [x] [Menu](https://v5.mantine.dev/core/menu/)
  - [x] [Modal](https://v5.mantine.dev/core/modal/)
  - [x] [Overlay](https://v5.mantine.dev/core/overlay/)
  - [x] [Popover](https://v5.mantine.dev/core/popover/)
  - [x] [Tooltip](https://v5.mantine.dev/core/tooltip/)
- [x] Typography
  - [x] [Blockquote](https://v5.mantine.dev/core/blockquote/)
  - [x] [Code](https://v5.mantine.dev/core/code/)
  - [x] [Highlight](https://v5.mantine.dev/core/highlight/)
  - [x] [List](https://v5.mantine.dev/core/list/)
  - [x] [Mark](https://v5.mantine.dev/core/mark/)
  - [x] [Table](https://v5.mantine.dev/core/table/)
  - [x] [Text](https://v5.mantine.dev/core/text/)
  - [x] [Title](https://v5.mantine.dev/core/title/)
  - [x] [TypographyStylesProvider](https://v5.mantine.dev/core/typography-styles-provider/)
- [x] Feedback
  - [x] [Alert](https://v5.mantine.dev/core/alert/)
  - [x] [Loader](https://v5.mantine.dev/core/loader/)
  - [x] [Notification](https://v5.mantine.dev/core/notification/)
  - [x] [Progress](https://v5.mantine.dev/core/progress/)
  - [x] [RingProgress](https://v5.mantine.dev/core/ring-progress/)
  - [x] [Skeleton](https://v5.mantine.dev/core/skeleton/)
- [x] Miscellaneous
  - [x] [Box](https://v5.mantine.dev/core/box/)
  - [x] [Collapse](https://v5.mantine.dev/core/collapse/)
  - [x] [Divider](https://v5.mantine.dev/core/divider/)
  - [x] [FocusTrap](https://v5.mantine.dev/core/focus-trap/)
  - [x] [Paper](https://v5.mantine.dev/core/paper/)
  - [x] [Portal](https://v5.mantine.dev/core/portal/)
  - [x] [ScrollArea](https://v5.mantine.dev/core/scroll-area/)
  - [x] [Transition](https://v5.mantine.dev/core/transition/)

### Hooks

```purescript
import Mantine.Hooks
```

- [ ] Theming
  - [x] [useMantineColorScheme](https://v5.mantine.dev/guides/dark-theme/#colorschemeprovider)
- [ ] State management
  - [ ] [useCounter](https://v5.mantine.dev/hooks/use-counter/)
  - [ ] [useDebouncedState](https://v5.mantine.dev/hooks/use-debounced-state/)
  - [ ] [useDebouncedValue](https://v5.mantine.dev/hooks/use-debounced-value/)
  - [ ] [useDisclosure](https://v5.mantine.dev/hooks/use-disclosure/)
  - [ ] [useId](https://v5.mantine.dev/hooks/use-id/)
  - [x] [useIdle](https://v5.mantine.dev/hooks/use-idle/)
  - [ ] [useInputState](https://v5.mantine.dev/hooks/use-input-state/)
  - [ ] [useInterval](https://v5.mantine.dev/hooks/use-interval/)
  - [ ] [useListState](https://v5.mantine.dev/hooks/use-list-state/)
  - [ ] [useLocalStorage](https://v5.mantine.dev/hooks/use-local-storage/)
  - [ ] [usePagination](https://v5.mantine.dev/hooks/use-pagination/)
  - [ ] [usePrevious](https://v5.mantine.dev/hooks/use-previous/)
  - [ ] [useQueue](https://v5.mantine.dev/hooks/use-queue/)
  - [ ] [useSetState](https://v5.mantine.dev/hooks/use-set-state/)
  - [ ] [useTimeout](https://v5.mantine.dev/hooks/use-timeout/)
  - [ ] [useToggle](https://v5.mantine.dev/hooks/use-toggle/)
  - [ ] [useUncontrolled](https://v5.mantine.dev/hooks/use-uncontrolled/)
  - [ ] [useValidatedState](https://v5.mantine.dev/hooks/use-validated-state/)
- [ ] UI and Dom
  - [x] [useClickOutside](https://v5.mantine.dev/hooks/use-click-outside/)
  - [x] [useColorScheme](https://v5.mantine.dev/hooks/use-color-scheme/)
  - [x] [useElementSize](https://v5.mantine.dev/hooks/use-element-size/)
  - [ ] [useEventListener](https://v5.mantine.dev/hooks/use-event-listener/)
  - [ ] [useFocusReturn](https://v5.mantine.dev/hooks/use-focus-return/)
  - [ ] [useFocusTrap](https://v5.mantine.dev/hooks/use-focus-trap/)
  - [x] [useFocusWithin](https://v5.mantine.dev/hooks/use-focus-within/)
  - [x] [useFullscreen](https://v5.mantine.dev/hooks/use-fullscreen/)
  - [x] [useHotkeys](https://v5.mantine.dev/hooks/use-hotkeys/)
  - [x] [useHover](https://v5.mantine.dev/hooks/use-hover/)
  - [ ] [useIntersection](https://v5.mantine.dev/hooks/use-intersection/)
  - [x] [useMediaQuery](https://v5.mantine.dev/hooks/use-media-query/)
  - [x] [useMouse](https://v5.mantine.dev/hooks/use-mouse/)
  - [x] [useMove](https://v5.mantine.dev/hooks/use-move/)
  - [x] [useReducedMotion](https://v5.mantine.dev/hooks/use-reduced-motion/)
  - [x] [useResizeObserver](https://v5.mantine.dev/hooks/use-resize-observer/)
  - [ ] [useScrollIntoView](https://v5.mantine.dev/hooks/use-scroll-into-view/)
  - [ ] [useScrollLock](https://v5.mantine.dev/hooks/use-scroll-lock/)
  - [x] [useViewportSize](https://v5.mantine.dev/hooks/use-viewport-size/)
- [ ] Utilities
  - [x] [useClipboard](https://v5.mantine.dev/hooks/use-clipboard/)
  - [x] [useDocumentTitle](https://v5.mantine.dev/hooks/use-document-title/)
  - [x] [useDocumentVisibility](https://v5.mantine.dev/hooks/use-document-visibility/)
  - [ ] [useEyeDropper](https://v5.mantine.dev/hooks/use-eye-dropper/)
  - [x] [useFavicon](https://v5.mantine.dev/hooks/use-favicon/)
  - [x] [useHash](https://v5.mantine.dev/hooks/use-hash/)
  - [ ] [useMergedRef](https://v5.mantine.dev/hooks/use-merged-ref/)
  - [ ] [useNetwork](https://v5.mantine.dev/hooks/use-network/)
  - [ ] [useOs](https://v5.mantine.dev/hooks/use-os/)
  - [x] [usePageLeave](https://v5.mantine.dev/hooks/use-page-leave/)
  - [ ] [useTextSelection](https://v5.mantine.dev/hooks/use-text-selection/)
  - [x] [useWindowEvent](https://v5.mantine.dev/hooks/use-window-event/)
  - [x] [useWindowScroll](https://v5.mantine.dev/hooks/use-window-scroll/)
- [ ] Lifecycle
  - [ ] [useDidUpdate](https://v5.mantine.dev/hooks/use-did-update/)
  - [ ] [useForceUpdate](https://v5.mantine.dev/hooks/use-force-update/)
  - [ ] [useIsomorphicEffect](https://v5.mantine.dev/hooks/use-isomorphic-effect/)
  - [ ] [useLogger](https://v5.mantine.dev/hooks/use-logger/)
  - [ ] [useShallowEffect](https://v5.mantine.dev/hooks/use-shallow-effect/)
