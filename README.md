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
  - [x] [AppShell](https://v6.mantine.dev/core/app-shell/)
  - [x] [AspectRatio](https://v6.mantine.dev/core/aspect-ratio/)
  - [x] [Center](https://v6.mantine.dev/core/center/)
  - [x] [Container](https://v6.mantine.dev/core/container/)
  - [x] [Flex](https://v6.mantine.dev/core/flex/)
  - [x] [Grid](https://v6.mantine.dev/core/grid/)
  - [x] [Group](https://v6.mantine.dev/core/group/)
  - [x] [MediaQuery](https://v6.mantine.dev/core/media-query/)
  - [x] [SimpleGrid](https://v6.mantine.dev/core/simple-grid/)
  - [x] [Space](https://v6.mantine.dev/core/space/)
  - [x] [Stack](https://v6.mantine.dev/core/stack/)
- [x] Buttons
  - [x] [ActionIcon](https://v6.mantine.dev/core/action-icon/)
  - [x] [Button](https://v6.mantine.dev/core/button/)
  - [x] [CloseButton](https://v6.mantine.dev/core/close-button/)
  - [x] [CopyButton](https://v6.mantine.dev/core/copy-button/)
  - [x] [FileButton](https://v6.mantine.dev/core/file-button/)
  - [x] [UnstyledButton](https://v6.mantine.dev/core/unstyled-button)
- [x] Inputs
  - [x] [Autocomplete](https://v6.mantine.dev/core/autocomplete/)
  - [x] [Checkbox](https://v6.mantine.dev/core/checkbox/)
  - [x] [Chip](https://v6.mantine.dev/core/chip/)
  - [x] [ColorInput](https://v6.mantine.dev/core/color-input/)
  - [x] [ColorPicker](https://v6.mantine.dev/core/color-picker/)
  - [x] [FileInput](https://v6.mantine.dev/core/file-input/)
  - [x] [Input](https://v6.mantine.dev/core/input/)
  - [x] [JsonInput](https://v6.mantine.dev/core/json-input/)
  - [x] [MultiSelect](https://v6.mantine.dev/core/multi-select/)
  - [x] [NativeSelect](https://v6.mantine.dev/core/native-select/)
  - [x] [NumberInput](https://v6.mantine.dev/core/number-input/)
  - [x] [PasswordInput](https://v6.mantine.dev/core/password-input/)
  - [x] [PinInput](https://v6.mantine.dev/core/password-input/)
  - [x] [Radio](https://v6.mantine.dev/core/radio/)
  - [x] [Rating](https://v6.mantine.dev/core/rating/)
  - [x] [SegmentedControl](https://v6.mantine.dev/core/segmented-control/)
  - [x] [Select](https://v6.mantine.dev/core/select/)
  - [x] [Slider](https://v6.mantine.dev/core/slider/)
  - [x] [Switch](https://v6.mantine.dev/core/switch/)
  - [x] [Textarea](https://v6.mantine.dev/core/textarea/)
  - [x] [TextInput](https://v6.mantine.dev/core/text-input/)
  - [x] [TransferList](https://v6.mantine.dev/core/transfer-list/)
- [x] Navigation (partially implemented)
  - [x] [Anchor](https://v6.mantine.dev/core/anchor/)
  - [x] [Breadcrumbs](https://v6.mantine.dev/core/breadcrumbs/)
  - [x] [Burger](https://v6.mantine.dev/core/burger/)
  - [x] [NavLink](https://v6.mantine.dev/core/nav-link/)
  - [x] [Pagination](https://v6.mantine.dev/core/pagination/) (partially implemented)
  - [x] [Stepper](https://v6.mantine.dev/core/stepper/)
  - [x] [Tabs](https://v6.mantine.dev/core/tabs/)
- [x] Data display
  - [x] [Accordion](https://v6.mantine.dev/core/accordion/)
  - [x] [Avatar](https://v6.mantine.dev/core/avatar/)
  - [x] [BackgroundImage](https://v6.mantine.dev/core/background-image/)
  - [x] [Badge](https://v6.mantine.dev/core/badge/)
  - [x] [Card](https://v6.mantine.dev/core/card/)
  - [x] [ColorSwatch](https://v6.mantine.dev/core/color-swatch/)
  - [x] [Image](https://v6.mantine.dev/core/image/)
  - [x] [Indicator](https://v6.mantine.dev/core/indicator/)
  - [x] [Kbd](https://v6.mantine.dev/core/kbd/)
  - [x] [Spoiler](https://v6.mantine.dev/core/spoiler/)
  - [x] [ThemeIcon](https://v6.mantine.dev/core/theme-icon/)
  - [x] [Timeline](https://v6.mantine.dev/core/timeline/)
- [x] Overlays
  - [x] [Affix](https://v6.mantine.dev/core/affix/)
  - [x] [Dialog](https://v6.mantine.dev/core/dialog/)
  - [x] [Drawer](https://v6.mantine.dev/core/drawer/)
  - [x] [HoverCard](https://v6.mantine.dev/core/hover-card/)
  - [x] [LoadingOverlay](https://v6.mantine.dev/core/loading-overlay/)
  - [x] [Menu](https://v6.mantine.dev/core/menu/)
  - [x] [Modal](https://v6.mantine.dev/core/modal/)
  - [x] [Overlay](https://v6.mantine.dev/core/overlay/)
  - [x] [Popover](https://v6.mantine.dev/core/popover/)
  - [x] [Tooltip](https://v6.mantine.dev/core/tooltip/)
- [x] Typography
  - [x] [Blockquote](https://v6.mantine.dev/core/blockquote/)
  - [x] [Code](https://v6.mantine.dev/core/code/)
  - [x] [Highlight](https://v6.mantine.dev/core/highlight/)
  - [x] [List](https://v6.mantine.dev/core/list/)
  - [x] [Mark](https://v6.mantine.dev/core/mark/)
  - [x] [Table](https://v6.mantine.dev/core/table/)
  - [x] [Text](https://v6.mantine.dev/core/text/)
  - [x] [Title](https://v6.mantine.dev/core/title/)
  - [x] [TypographyStylesProvider](https://v6.mantine.dev/core/typography-styles-provider/)
- [x] Feedback
  - [x] [Alert](https://v6.mantine.dev/core/alert/)
  - [x] [Loader](https://v6.mantine.dev/core/loader/)
  - [x] [Notification](https://v6.mantine.dev/core/notification/)
  - [x] [Progress](https://v6.mantine.dev/core/progress/)
  - [x] [RingProgress](https://v6.mantine.dev/core/ring-progress/)
  - [x] [Skeleton](https://v6.mantine.dev/core/skeleton/)
- [x] Miscellaneous
  - [x] [Box](https://v6.mantine.dev/core/box/)
  - [x] [Collapse](https://v6.mantine.dev/core/collapse/)
  - [x] [Divider](https://v6.mantine.dev/core/divider/)
  - [x] [FocusTrap](https://v6.mantine.dev/core/focus-trap/)
  - [x] [Paper](https://v6.mantine.dev/core/paper/)
  - [x] [Portal](https://v6.mantine.dev/core/portal/)
  - [x] [ScrollArea](https://v6.mantine.dev/core/scroll-area/)
  - [x] [Transition](https://v6.mantine.dev/core/transition/)

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
