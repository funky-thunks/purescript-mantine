# Purescript bindings for mantine

Bindings to use [Mantine](mantine.dev) components from a Purescript codebase.

Currently supporting mantine v5.

:warning: This is work-in-progress and this is very unstable.

## Progress

### Components

```purescript
import Mantine.Core
```

- [ ] Layout
  - [ ] [AppShell](https://v6.mantine.dev/core/app-shell/)
  - [ ] [AspectRatio](https://v6.mantine.dev/core/aspect-ratio/)
  - [ ] [Center](https://v6.mantine.dev/core/center/)
  - [ ] [Container](https://v6.mantine.dev/core/container/)
  - [ ] [Flex](https://v6.mantine.dev/core/flex/)
  - [ ] [Grid](https://v6.mantine.dev/core/grid/)
  - [ ] [Group](https://v6.mantine.dev/core/group/)
  - [ ] [MediaQuery](https://v6.mantine.dev/core/media-query/)
  - [ ] [SimpleGrid](https://v6.mantine.dev/core/simple-grid/)
  - [ ] [Space](https://v6.mantine.dev/core/space/)
  - [ ] [Stack](https://v6.mantine.dev/core/stack/)
- [ ] Buttons
  - [ ] [ActionIcon](https://v6.mantine.dev/core/action-icon/)
  - [ ] [Button](https://v6.mantine.dev/core/button/)
  - [ ] [CloseButton](https://v6.mantine.dev/core/close-button/)
  - [ ] [CopyButton](https://v6.mantine.dev/core/copy-button/)
  - [ ] [FileButton](https://v6.mantine.dev/core/file-button/)
  - [ ] [UnstyledButton](https://v6.mantine.dev/core/unstyled-button)
- [ ] Inputs
  - [ ] [Autocomplete](https://v6.mantine.dev/core/autocomplete/)
  - [ ] [Checkbox](https://v6.mantine.dev/core/checkbox/)
  - [ ] [Chip](https://v6.mantine.dev/core/chip/)
  - [ ] [ColorInput](https://v6.mantine.dev/core/color-input/)
  - [ ] [ColorPicker](https://v6.mantine.dev/core/color-picker/)
  - [ ] [FileInput](https://v6.mantine.dev/core/file-input/)
  - [ ] [Input](https://v6.mantine.dev/core/input/)
  - [ ] [JsonInput](https://v6.mantine.dev/core/json-input/)
  - [ ] [MultiSelect](https://v6.mantine.dev/core/multi-select/)
  - [ ] [NativeSelect](https://v6.mantine.dev/core/native-select/)
  - [ ] [NumberInput](https://v6.mantine.dev/core/number-input/)
  - [ ] [PasswordInput](https://v6.mantine.dev/core/password-input/)
  - [ ] [PinInput](https://v6.mantine.dev/core/password-input/)
  - [ ] [Radio](https://v6.mantine.dev/core/radio/)
  - [ ] [Rating](https://v6.mantine.dev/core/rating/)
  - [ ] [SegmentedControl](https://v6.mantine.dev/core/segmented-control/)
  - [ ] [Select](https://v6.mantine.dev/core/select/)
  - [ ] [Slider](https://v6.mantine.dev/core/slider/)
  - [ ] [Switch](https://v6.mantine.dev/core/switch/)
  - [ ] [Textarea](https://v6.mantine.dev/core/textarea/)
  - [ ] [TextInput](https://v6.mantine.dev/core/text-input/)
  - [ ] [TransferList](https://v6.mantine.dev/core/transfer-list/)
- [ ] Navigation
  - [ ] [Anchor](https://v6.mantine.dev/core/anchor/)
  - [ ] [Breadcrumbs](https://v6.mantine.dev/core/breadcrumbs/)
  - [ ] [Burger](https://v6.mantine.dev/core/burger/)
  - [ ] [NavLink](https://v6.mantine.dev/core/nav-link/)
  - [ ] [Pagination](https://v6.mantine.dev/core/pagination/)
  - [ ] [Stepper](https://v6.mantine.dev/core/stepper/)
  - [ ] [Tabs](https://v6.mantine.dev/core/tabs/)
- [ ] Data display
  - [ ] [Accordion](https://v6.mantine.dev/core/accordion/)
  - [ ] [Avatar](https://v6.mantine.dev/core/avatar/)
  - [ ] [BackgroundImage](https://v6.mantine.dev/core/background-image/)
  - [ ] [Badge](https://v6.mantine.dev/core/badge/)
  - [ ] [Card](https://v6.mantine.dev/core/card/)
  - [ ] [ColorSwatch](https://v6.mantine.dev/core/color-swatch/)
  - [ ] [Image](https://v6.mantine.dev/core/image/)
  - [ ] [Indicator](https://v6.mantine.dev/core/indicator/)
  - [ ] [Kbd](https://v6.mantine.dev/core/kbd/)
  - [ ] [Spoiler](https://v6.mantine.dev/core/spoiler/)
  - [ ] [ThemeIcon](https://v6.mantine.dev/core/theme-icon/)
  - [ ] [Timeline](https://v6.mantine.dev/core/timeline/)
- [ ] Overlays
  - [ ] [Affix](https://v6.mantine.dev/core/affix/)
  - [ ] [Dialog](https://v6.mantine.dev/core/dialog/)
  - [ ] [Drawer](https://v6.mantine.dev/core/drawer/)
  - [ ] [HoverCard](https://v6.mantine.dev/core/hover-card/)
  - [ ] [LoadingOverlay](https://v6.mantine.dev/core/loading-overlay/)
  - [ ] [Menu](https://v6.mantine.dev/core/menu/)
  - [ ] [Modal](https://v6.mantine.dev/core/modal/)
  - [ ] [Overlay](https://v6.mantine.dev/core/overlay/)
  - [ ] [Popover](https://v6.mantine.dev/core/popover/)
  - [ ] [Tooltip](https://v6.mantine.dev/core/tooltip/)
- [ ] Typography
  - [ ] [Blockquote](https://v6.mantine.dev/core/blockquote/)
  - [ ] [Code](https://v6.mantine.dev/core/code/)
  - [ ] [Highlight](https://v6.mantine.dev/core/highlight/)
  - [ ] [List](https://v6.mantine.dev/core/list/)
  - [ ] [Mark](https://v6.mantine.dev/core/mark/)
  - [ ] [Table](https://v6.mantine.dev/core/table/)
  - [ ] [Text](https://v6.mantine.dev/core/text/)
  - [ ] [Title](https://v6.mantine.dev/core/title/)
  - [ ] [TypographyStylesProvider](https://v6.mantine.dev/core/typography-styles-provider/)
- [ ] Feedback
  - [ ] [Alert](https://v6.mantine.dev/core/alert/)
  - [ ] [Loader](https://v6.mantine.dev/core/loader/)
  - [ ] [Notification](https://v6.mantine.dev/core/notification/)
  - [ ] [Progress](https://v6.mantine.dev/core/progress/)
  - [ ] [RingProgress](https://v6.mantine.dev/core/ring-progress/)
  - [ ] [Skeleton](https://v6.mantine.dev/core/skeleton/)
- [ ] Miscellaneous
  - [ ] [Box](https://v6.mantine.dev/core/box/)
  - [ ] [Collapse](https://v6.mantine.dev/core/collapse/)
  - [ ] [Divider](https://v6.mantine.dev/core/divider/)
  - [ ] [FocusTrap](https://v6.mantine.dev/core/focus-trap/)
  - [ ] [Paper](https://v6.mantine.dev/core/paper/)
  - [ ] [Portal](https://v6.mantine.dev/core/portal/)
  - [ ] [ScrollArea](https://v6.mantine.dev/core/scroll-area/)
  - [ ] [Transition](https://v6.mantine.dev/core/transition/)

### Hooks

```purescript
import Mantine.Hooks
```

- [?] Theming
  - [x] [useMantineColorScheme](https://v6.mantine.dev/guides/dark-theme/#colorschemeprovider)
- [ ] State management
  - [ ] [useCounter](https://v6.mantine.dev/hooks/use-counter/)
  - [ ] [useDebouncedState](https://v6.mantine.dev/hooks/use-debounced-state/)
  - [ ] [useDebouncedValue](https://v6.mantine.dev/hooks/use-debounced-value/)
  - [ ] [useDisclosure](https://v6.mantine.dev/hooks/use-disclosure/)
  - [ ] [useId](https://v6.mantine.dev/hooks/use-id/)
  - [x] [useIdle](https://v6.mantine.dev/hooks/use-idle/)
  - [ ] [useInputState](https://v6.mantine.dev/hooks/use-input-state/)
  - [ ] [useInterval](https://v6.mantine.dev/hooks/use-interval/)
  - [ ] [useListState](https://v6.mantine.dev/hooks/use-list-state/)
  - [ ] [useLocalStorage](https://v6.mantine.dev/hooks/use-local-storage/)
  - [ ] [usePagination](https://v6.mantine.dev/hooks/use-pagination/)
  - [ ] [usePrevious](https://v6.mantine.dev/hooks/use-previous/)
  - [ ] [useQueue](https://v6.mantine.dev/hooks/use-queue/)
  - [ ] [useSetState](https://v6.mantine.dev/hooks/use-set-state/)
  - [ ] [useTimeout](https://v6.mantine.dev/hooks/use-timeout/)
  - [ ] [useToggle](https://v6.mantine.dev/hooks/use-toggle/)
  - [ ] [useUncontrolled](https://v6.mantine.dev/hooks/use-uncontrolled/)
  - [ ] [useValidatedState](https://v6.mantine.dev/hooks/use-validated-state/)
- [ ] UI and Dom
  - [x] [useClickOutside](https://v6.mantine.dev/hooks/use-click-outside/)
  - [x] [useColorScheme](https://v6.mantine.dev/hooks/use-color-scheme/)
  - [x] [useElementSize](https://v6.mantine.dev/hooks/use-element-size/)
  - [ ] [useEventListener](https://v6.mantine.dev/hooks/use-event-listener/)
  - [ ] [useFocusReturn](https://v6.mantine.dev/hooks/use-focus-return/)
  - [ ] [useFocusTrap](https://v6.mantine.dev/hooks/use-focus-trap/)
  - [x] [useFocusWithin](https://v6.mantine.dev/hooks/use-focus-within/)
  - [x] [useFullscreen](https://v6.mantine.dev/hooks/use-fullscreen/)
  - [x] [useHotkeys](https://v6.mantine.dev/hooks/use-hotkeys/)
  - [x] [useHover](https://v6.mantine.dev/hooks/use-hover/)
  - [ ] [useIntersection](https://v6.mantine.dev/hooks/use-intersection/)
  - [x] [useMediaQuery](https://v6.mantine.dev/hooks/use-media-query/)
  - [x] [useMouse](https://v6.mantine.dev/hooks/use-mouse/)
  - [x] [useMove](https://v6.mantine.dev/hooks/use-move/)
  - [x] [useReducedMotion](https://v6.mantine.dev/hooks/use-reduced-motion/)
  - [x] [useResizeObserver](https://v6.mantine.dev/hooks/use-resize-observer/)
  - [ ] [useScrollIntoView](https://v6.mantine.dev/hooks/use-scroll-into-view/)
  - [x] [useViewportSize](https://v6.mantine.dev/hooks/use-viewport-size/)
- [ ] Utilities
  - [x] [useClipboard](https://v6.mantine.dev/hooks/use-clipboard/)
  - [x] [useDocumentTitle](https://v6.mantine.dev/hooks/use-document-title/)
  - [x] [useDocumentVisibility](https://v6.mantine.dev/hooks/use-document-visibility/)
  - [ ] [useEyeDropper](https://v6.mantine.dev/hooks/use-eye-dropper/)
  - [x] [useFavicon](https://v6.mantine.dev/hooks/use-favicon/)
  - [x] [useHash](https://v6.mantine.dev/hooks/use-hash/)
  - [ ] [useHeadroom](https://v6.mantine.dev/hooks/use-headroom/)
  - [ ] [useMergedRef](https://v6.mantine.dev/hooks/use-merged-ref/)
  - [ ] [useNetwork](https://v6.mantine.dev/hooks/use-network/)
  - [ ] [useOs](https://v6.mantine.dev/hooks/use-os/)
  - [x] [usePageLeave](https://v6.mantine.dev/hooks/use-page-leave/)
  - [ ] [useTextSelection](https://v6.mantine.dev/hooks/use-text-selection/)
  - [x] [useWindowEvent](https://v6.mantine.dev/hooks/use-window-event/)
  - [x] [useWindowScroll](https://v6.mantine.dev/hooks/use-window-scroll/)
- [ ] Lifecycle
  - [ ] [useDidUpdate](https://v6.mantine.dev/hooks/use-did-update/)
  - [ ] [useForceUpdate](https://v6.mantine.dev/hooks/use-force-update/)
  - [ ] [useIsomorphicEffect](https://v6.mantine.dev/hooks/use-isomorphic-effect/)
  - [ ] [useLogger](https://v6.mantine.dev/hooks/use-logger/)
  - [ ] [useShallowEffect](https://v6.mantine.dev/hooks/use-shallow-effect/)
