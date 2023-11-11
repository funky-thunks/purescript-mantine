# Purescript bindings for mantine

Bindings to use [Mantine](mantine.dev) components from a Purescript codebase.

Currently supporting mantine v7.2.1.

:warning: This is work-in-progress and this is very unstable.

## Progress

### Core

```purescript
import Mantine.Core
```

- [x] Layout
  - [x] [AppShell](https://mantine.dev/core/app-shell/)
  - [x] [AspectRatio](https://mantine.dev/core/aspect-ratio/)
  - [x] [Center](https://mantine.dev/core/center/)
  - [x] [Container](https://mantine.dev/core/container/)
  - [x] [Flex](https://mantine.dev/core/flex/)
  - [x] [Grid](https://mantine.dev/core/grid/)
  - [x] [Group](https://mantine.dev/core/group/)
  - [x] [SimpleGrid](https://mantine.dev/core/simple-grid/)
  - [x] [Space](https://mantine.dev/core/space/)
  - [x] [Stack](https://mantine.dev/core/stack/)
- [x] Inputs
  - [x] [Checkbox](https://mantine.dev/core/checkbox/)
  - [x] [Chip](https://mantine.dev/core/chip/)
  - [x] [ColorInput](https://mantine.dev/core/color-input/)
  - [x] [ColorPicker](https://mantine.dev/core/color-picker/)
  - [x] [Fieldset](https://mantine.dev/core/fieldset/)
  - [x] [FileInput](https://mantine.dev/core/file-input/)
  - [x] [Input](https://mantine.dev/core/input/)
  - [x] [JsonInput](https://mantine.dev/core/json-input/)
  - [x] [NativeSelect](https://mantine.dev/core/native-select/)
  - [x] [NumberInput](https://mantine.dev/core/number-input/)
  - [x] [PasswordInput](https://mantine.dev/core/password-input/)
  - [x] [PinInput](https://mantine.dev/core/password-input/)
  - [x] [Radio](https://mantine.dev/core/radio/)
  - [x] [Rating](https://mantine.dev/core/rating/)
  - [x] [SegmentedControl](https://mantine.dev/core/segmented-control/)
  - [x] [Slider](https://mantine.dev/core/slider/)
  - [x] [Switch](https://mantine.dev/core/switch/)
  - [x] [Textarea](https://mantine.dev/core/textarea/)
  - [x] [TextInput](https://mantine.dev/core/text-input/)
- [x] Combobox
  - [x] [Autocomplete](https://mantine.dev/core/autocomplete/)
  - [x] [Combobox](https://mantine.dev/core/combobox/)
  - [x] [MultiSelect](https://mantine.dev/core/multi-select/)
  - [x] [Pill](https://mantine.dev/core/pill/)
  - [x] [PillsInput](https://mantine.dev/core/pills-input/)
  - [x] [Select](https://mantine.dev/core/select/)
  - [x] [TagsInput](https://mantine.dev/core/tags-input/)
- [x] Buttons
  - [x] [ActionIcon](https://mantine.dev/core/action-icon/)
  - [x] [Button](https://mantine.dev/core/button/)
  - [x] [CloseButton](https://mantine.dev/core/close-button/)
  - [x] [CopyButton](https://mantine.dev/core/copy-button/)
  - [x] [FileButton](https://mantine.dev/core/file-button/)
  - [x] [UnstyledButton](https://mantine.dev/core/unstyled-button)
- [x] Navigation (partially implemented)
  - [x] [Anchor](https://mantine.dev/core/anchor/)
  - [x] [Breadcrumbs](https://mantine.dev/core/breadcrumbs/)
  - [x] [Burger](https://mantine.dev/core/burger/)
  - [x] [NavLink](https://mantine.dev/core/nav-link/)
  - [x] [Pagination](https://mantine.dev/core/pagination/) (partially implemented)
  - [x] [Stepper](https://mantine.dev/core/stepper/)
  - [x] [Tabs](https://mantine.dev/core/tabs/)
- [x] Feedback
  - [x] [Alert](https://mantine.dev/core/alert/)
  - [x] [Loader](https://mantine.dev/core/loader/)
  - [x] [Notification](https://mantine.dev/core/notification/)
  - [x] [Progress](https://mantine.dev/core/progress/)
  - [x] [RingProgress](https://mantine.dev/core/ring-progress/)
  - [x] [Skeleton](https://mantine.dev/core/skeleton/)
- [x] Overlays
  - [x] [Affix](https://mantine.dev/core/affix/)
  - [x] [Dialog](https://mantine.dev/core/dialog/)
  - [x] [Drawer](https://mantine.dev/core/drawer/)
  - [x] [HoverCard](https://mantine.dev/core/hover-card/)
  - [x] [LoadingOverlay](https://mantine.dev/core/loading-overlay/)
  - [x] [Menu](https://mantine.dev/core/menu/)
  - [x] [Modal](https://mantine.dev/core/modal/)
  - [x] [Overlay](https://mantine.dev/core/overlay/)
  - [x] [Popover](https://mantine.dev/core/popover/)
  - [x] [Tooltip](https://mantine.dev/core/tooltip/)
- [x] Data display
  - [x] [Accordion](https://mantine.dev/core/accordion/)
  - [x] [Avatar](https://mantine.dev/core/avatar/)
  - [x] [BackgroundImage](https://mantine.dev/core/background-image/)
  - [x] [Badge](https://mantine.dev/core/badge/)
  - [x] [Card](https://mantine.dev/core/card/)
  - [x] [ColorSwatch](https://mantine.dev/core/color-swatch/)
  - [x] [Image](https://mantine.dev/core/image/)
  - [x] [Indicator](https://mantine.dev/core/indicator/)
  - [x] [Kbd](https://mantine.dev/core/kbd/)
  - [x] [NumberFormatter](https://mantine.dev/core/number-formatter/)
  - [x] [Spoiler](https://mantine.dev/core/spoiler/)
  - [x] [ThemeIcon](https://mantine.dev/core/theme-icon/)
  - [x] [Timeline](https://mantine.dev/core/timeline/)
- [x] Typography
  - [x] [Blockquote](https://mantine.dev/core/blockquote/)
  - [x] [Code](https://mantine.dev/core/code/)
  - [x] [Highlight](https://mantine.dev/core/highlight/)
  - [x] [List](https://mantine.dev/core/list/)
  - [x] [Mark](https://mantine.dev/core/mark/)
  - [x] [Table](https://mantine.dev/core/table/)
  - [x] [Text](https://mantine.dev/core/text/)
  - [x] [Title](https://mantine.dev/core/title/)
  - [x] [TypographyStylesProvider](https://mantine.dev/core/typography-styles-provider/)
- [x] Miscellaneous
  - [x] [Box](https://mantine.dev/core/box/)
  - [x] [Collapse](https://mantine.dev/core/collapse/)
  - [x] [Divider](https://mantine.dev/core/divider/)
  - [x] [FocusTrap](https://mantine.dev/core/focus-trap/)
  - [x] [Paper](https://mantine.dev/core/paper/)
  - [x] [Portal](https://mantine.dev/core/portal/)
  - [x] [ScrollArea](https://mantine.dev/core/scroll-area/)
  - [x] [Transition](https://mantine.dev/core/transition/)
  - [x] [VisuallyHidden](https://mantine.dev/core/visually-hidden/)

### Hooks

```purescript
import Mantine.Hooks
```

- [x] Theming
  - [x] [useMantineColorScheme](https://mantine.dev/guides/dark-theme/#colorschemeprovider)
- [ ] UI and Dom
  - [x] [useClickOutside](https://mantine.dev/hooks/use-click-outside/)
  - [x] [useColorScheme](https://mantine.dev/hooks/use-color-scheme/)
  - [x] [useElementSize](https://mantine.dev/hooks/use-element-size/)
  - [ ] [useEventListener](https://mantine.dev/hooks/use-event-listener/)
  - [ ] [useFocusReturn](https://mantine.dev/hooks/use-focus-return/)
  - [ ] [useFocusTrap](https://mantine.dev/hooks/use-focus-trap/)
  - [x] [useFocusWithin](https://mantine.dev/hooks/use-focus-within/)
  - [x] [useFullscreen](https://mantine.dev/hooks/use-fullscreen/)
  - [x] [useHotkeys](https://mantine.dev/hooks/use-hotkeys/)
  - [x] [useHover](https://mantine.dev/hooks/use-hover/)
  - [ ] [useIntersection](https://mantine.dev/hooks/use-intersection/)
  - [x] [useMediaQuery](https://mantine.dev/hooks/use-media-query/)
  - [x] [useMouse](https://mantine.dev/hooks/use-mouse/)
  - [x] [useMove](https://mantine.dev/hooks/use-move/)
  - [x] [useReducedMotion](https://mantine.dev/hooks/use-reduced-motion/)
  - [x] [useResizeObserver](https://mantine.dev/hooks/use-resize-observer/)
  - [ ] [useScrollIntoView](https://mantine.dev/hooks/use-scroll-into-view/)
  - [x] [useViewportSize](https://mantine.dev/hooks/use-viewport-size/)
  - [x] [useWindowEvent](https://mantine.dev/hooks/use-window-event/)
  - [x] [useWindowScroll](https://mantine.dev/hooks/use-window-scroll/)
- [ ] State management
  - [ ] [useCounter](https://mantine.dev/hooks/use-counter/)
  - [ ] [useDebouncedState](https://mantine.dev/hooks/use-debounced-state/)
  - [ ] [useDebouncedValue](https://mantine.dev/hooks/use-debounced-value/)
  - [ ] [useDisclosure](https://mantine.dev/hooks/use-disclosure/)
  - [ ] [useId](https://mantine.dev/hooks/use-id/)
  - [ ] [useInputState](https://mantine.dev/hooks/use-input-state/)
  - [ ] [useListState](https://mantine.dev/hooks/use-list-state/)
  - [ ] [useLocalStorage](https://mantine.dev/hooks/use-local-storage/)
  - [ ] [usePagination](https://mantine.dev/hooks/use-pagination/)
  - [ ] [usePrevious](https://mantine.dev/hooks/use-previous/)
  - [ ] [useQueue](https://mantine.dev/hooks/use-queue/)
  - [ ] [useSetState](https://mantine.dev/hooks/use-set-state/)
  - [ ] [useToggle](https://mantine.dev/hooks/use-toggle/)
  - [ ] [useUncontrolled](https://mantine.dev/hooks/use-uncontrolled/)
  - [ ] [useValidatedState](https://mantine.dev/hooks/use-validated-state/)
- [ ] Utilities
  - [x] [useClipboard](https://mantine.dev/hooks/use-clipboard/)
  - [x] [useDocumentTitle](https://mantine.dev/hooks/use-document-title/)
  - [x] [useDocumentVisibility](https://mantine.dev/hooks/use-document-visibility/)
  - [ ] [useEyeDropper](https://mantine.dev/hooks/use-eye-dropper/)
  - [x] [useFavicon](https://mantine.dev/hooks/use-favicon/)
  - [x] [useHash](https://mantine.dev/hooks/use-hash/)
  - [x] [useHeadroom](https://mantine.dev/hooks/use-headroom/)
  - [x] [useIdle](https://mantine.dev/hooks/use-idle/)
  - [ ] [useInterval](https://mantine.dev/hooks/use-interval/)
  - [ ] [useMergedRef](https://mantine.dev/hooks/use-merged-ref/)
  - [x] [useNetwork](https://mantine.dev/hooks/use-network/)
  - [x] [useOs](https://mantine.dev/hooks/use-os/)
  - [x] [usePageLeave](https://mantine.dev/hooks/use-page-leave/)
  - [ ] [useTextSelection](https://mantine.dev/hooks/use-text-selection/)
  - [ ] [useTimeout](https://mantine.dev/hooks/use-timeout/)
- [ ] Lifecycle
  - [ ] [useDidUpdate](https://mantine.dev/hooks/use-did-update/)
  - [ ] [useForceUpdate](https://mantine.dev/hooks/use-force-update/)
  - [ ] [useIsomorphicEffect](https://mantine.dev/hooks/use-isomorphic-effect/)
  - [ ] [useLogger](https://mantine.dev/hooks/use-logger/)
  - [ ] [useShallowEffect](https://mantine.dev/hooks/use-shallow-effect/)

### Dates

```purescript
import Mantine.Dates
```

- [x] [DatesProvider](dates/dates-provider/)
- [x] [Calendar](dates/calendar/)
- [x] [DateInput](dates/date-input/)
- [x] [DateTimePicker](dates/date-time-picker/)
- [x] [DatePicker](dates/date-picker/)
- [x] [DatePickerInput](dates/date-picker-input/)
- [x] [MonthPicker](dates/month-picker/)
- [x] [MonthPickerInput](dates/month-picker-input/)
- [x] [YearPicker](dates/year-picker/)
- [x] [YearPickerInput](dates/year-picker-input/)
- [x] [TimeInput](dates/time-input/)
