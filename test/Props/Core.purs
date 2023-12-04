module Test.Props.Core where

import Mantine.Core as MC
import Mantine.FFI (toNative)
import Web.File.File (File)

accordionProps :: Record (MC.Props_Accordion String) -> Record (MC.Props_AccordionImpl String)
accordionProps = toNative

accordionControlProps :: Record MC.Props_AccordionControl -> Record MC.Props_AccordionControlImpl
accordionControlProps = toNative

accordionItemProps :: Record MC.Props_AccordionItem -> Record MC.Props_AccordionItemImpl
accordionItemProps = toNative

actionIconProps :: Record MC.Props_ActionIcon -> Record MC.Props_ActionIconImpl
actionIconProps = toNative

actionIconGroupProps :: Record MC.Props_ActionIconGroup -> Record MC.Props_ActionIconGroupImpl
actionIconGroupProps = toNative

affixProps :: Record MC.Props_Affix -> Record MC.Props_AffixImpl
affixProps = toNative

alertProps :: Record MC.Props_Alert -> Record MC.Props_AlertImpl
alertProps = toNative

anchorProps :: Record MC.Props_Anchor -> Record MC.Props_AnchorImpl
anchorProps = toNative

appShellProps :: Record MC.Props_AppShell -> Record MC.Props_AppShellImpl
appShellProps = toNative

appShellComponentProps :: Record MC.Props_AppShellComponent -> Record MC.Props_AppShellComponentImpl
appShellComponentProps = toNative

appShellMainProps :: Record MC.Props_AppShellMain -> Record MC.Props_AppShellMainImpl
appShellMainProps = toNative

appShellSectionProps :: Record MC.Props_AppShellSection -> Record MC.Props_AppShellSectionImpl
appShellSectionProps = toNative

aspectRatioProps :: Record MC.Props_AspectRatio -> Record MC.Props_AspectRatioImpl
aspectRatioProps = toNative

autocompleteProps :: Record MC.Props_Autocomplete -> Record MC.Props_AutocompleteImpl
autocompleteProps = toNative

avatarProps :: Record MC.Props_Avatar -> Record MC.Props_AvatarImpl
avatarProps = toNative

avatarGroupProps :: Record MC.Props_AvatarGroup -> Record MC.Props_AvatarGroupImpl
avatarGroupProps = toNative

backgroundImageProps :: Record MC.Props_BackgroundImage -> Record MC.Props_BackgroundImageImpl
backgroundImageProps = toNative

badgeProps :: Record MC.Props_Badge -> Record MC.Props_BadgeImpl
badgeProps = toNative

blockquoteProps :: Record MC.Props_Blockquote -> Record MC.Props_BlockquoteImpl
blockquoteProps = toNative

boxProps :: Record MC.Props_Box -> Record MC.Props_BoxImpl
boxProps = toNative

breadcrumbsProps :: Record MC.Props_Breadcrumbs -> Record MC.Props_BreadcrumbsImpl
breadcrumbsProps = toNative

burgerProps :: Record MC.Props_Burger -> Record MC.Props_BurgerImpl
burgerProps = toNative

buttonProps :: Record MC.Props_Button -> Record MC.Props_ButtonImpl
buttonProps = toNative

buttonGroupProps :: Record MC.Props_ButtonGroup -> Record MC.Props_ButtonGroupImpl
buttonGroupProps = toNative

cardProps :: Record MC.Props_Card -> Record MC.Props_CardImpl
cardProps = toNative

cardSectionProps :: Record MC.Props_CardSection -> Record MC.Props_CardSectionImpl
cardSectionProps = toNative

centerProps :: Record MC.Props_Center -> Record MC.Props_CenterImpl
centerProps = toNative

checkboxProps :: Record MC.Props_Checkbox -> Record MC.Props_CheckboxImpl
checkboxProps = toNative

checkboxGroupProps :: Record MC.Props_CheckboxGroup -> Record MC.Props_CheckboxGroupImpl
checkboxGroupProps = toNative

chipProps :: Record MC.Props_Chip -> Record MC.Props_ChipImpl
chipProps = toNative

closeButtonProps :: Record MC.Props_CloseButton -> Record MC.Props_CloseButtonImpl
closeButtonProps = toNative

codeProps :: Record MC.Props_Code -> Record MC.Props_CodeImpl
codeProps = toNative

collapseProps :: Record MC.Props_Collapse -> Record MC.Props_CollapseImpl
collapseProps = toNative

colorInputProps :: Record MC.Props_ColorInput -> Record MC.Props_ColorInputImpl
colorInputProps = toNative

colorPickerProps :: Record MC.Props_ColorPicker -> Record MC.Props_ColorPickerImpl
colorPickerProps = toNative

colorSwatchProps :: Record MC.Props_ColorSwatch -> Record MC.Props_ColorSwatchImpl
colorSwatchProps = toNative

comboboxProps :: Record MC.Props_Combobox -> Record MC.Props_ComboboxImpl
comboboxProps = toNative

comboboxDropdownProps :: Record MC.Props_ComboboxDropdown -> Record MC.Props_ComboboxDropdownImpl
comboboxDropdownProps = toNative

comboboxDropdownTargetProps :: Record MC.Props_ComboboxDropdownTarget -> Record MC.Props_ComboboxDropdownTargetImpl
comboboxDropdownTargetProps = toNative

comboboxEventsTargetProps :: Record MC.Props_ComboboxEventsTarget -> Record MC.Props_ComboboxEventsTargetImpl
comboboxEventsTargetProps = toNative

comboboxGroupProps :: Record MC.Props_ComboboxGroup -> Record MC.Props_ComboboxGroupImpl
comboboxGroupProps = toNative

comboboxOptionProps :: Record MC.Props_ComboboxOption -> Record MC.Props_ComboboxOptionImpl
comboboxOptionProps = toNative

comboboxOptionsProps :: Record MC.Props_ComboboxOptions -> Record MC.Props_ComboboxOptionsImpl
comboboxOptionsProps = toNative

comboboxTargetProps :: Record MC.Props_ComboboxTarget -> Record MC.Props_ComboboxTargetImpl
comboboxTargetProps = toNative

containerProps :: Record MC.Props_Container -> Record MC.Props_ContainerImpl
containerProps = toNative

copyButtonProps :: Record MC.Props_CopyButton -> Record MC.Props_CopyButtonImpl
copyButtonProps = toNative

dialogProps :: Record MC.Props_Dialog -> Record MC.Props_DialogImpl
dialogProps = toNative

dividerProps :: Record MC.Props_Divider -> Record MC.Props_DividerImpl
dividerProps = toNative

drawerProps :: Record MC.Props_Drawer -> Record MC.Props_DrawerImpl
drawerProps = toNative

fieldsetProps :: Record MC.Props_Fieldset -> Record MC.Props_FieldsetImpl
fieldsetProps = toNative

fileButtonProps :: Record (MC.Props_FileButton File) -> Record (MC.Props_FileButtonImpl File)
fileButtonProps = toNative

fileInputProps :: Record MC.Props_FileInput -> Record MC.Props_FileInputImpl
fileInputProps = toNative

flexProps :: Record MC.Props_Flex -> Record MC.Props_FlexImpl
flexProps = toNative

focusTrapProps :: Record MC.Props_FocusTrap -> Record MC.Props_FocusTrap
focusTrapProps = toNative

gridProps :: Record MC.Props_Grid -> Record MC.Props_GridImpl
gridProps = toNative

gridColProps :: Record MC.Props_GridCol -> Record MC.Props_GridColImpl
gridColProps = toNative

groupProps :: Record MC.Props_Group -> Record MC.Props_GroupImpl
groupProps = toNative

highlightProps :: Record MC.Props_Highlight -> Record MC.Props_HighlightImpl
highlightProps = toNative

hoverCardProps :: Record MC.Props_HoverCard -> Record MC.Props_HoverCardImpl
hoverCardProps = toNative

hoverTargetProps :: Record MC.Props_HoverTarget -> Record MC.Props_HoverTargetImpl
hoverTargetProps = toNative

hoveringDropdownProps :: Record MC.Props_HoveringDropdown -> Record MC.Props_HoveringDropdownImpl
hoveringDropdownProps = toNative

imageProps :: Record MC.Props_Image -> Record MC.Props_ImageImpl
imageProps = toNative

indicatorProps :: Record MC.Props_Indicator -> Record MC.Props_IndicatorImpl
indicatorProps = toNative

inputProps :: Record MC.Props_Input -> Record MC.Props_InputImpl
inputProps = toNative

inputDescriptionProps :: Record MC.Props_InputDescription -> Record MC.Props_InputDescriptionImpl
inputDescriptionProps = toNative

inputErrorProps :: Record MC.Props_InputError -> Record MC.Props_InputErrorImpl
inputErrorProps = toNative

inputLabelProps :: Record MC.Props_InputLabel -> Record MC.Props_InputLabelImpl
inputLabelProps = toNative

inputWrapperProps :: Record MC.Props_InputWrapper -> Record MC.Props_InputWrapperImpl
inputWrapperProps = toNative

jsonInputProps :: Record MC.Props_JsonInput -> Record MC.Props_JsonInputImpl
jsonInputProps = toNative

kbdProps :: Record MC.Props_Kbd -> Record MC.Props_KbdImpl
kbdProps = toNative

listProps :: Record MC.Props_List -> Record MC.Props_ListImpl
listProps = toNative

listItemProps :: Record MC.Props_ListItem -> Record MC.Props_ListItemImpl
listItemProps = toNative

loaderProps :: Record MC.Props_Loader -> Record MC.Props_LoaderImpl
loaderProps = toNative

loadingOverlayProps :: Record MC.Props_LoadingOverlay -> Record MC.Props_LoadingOverlayImpl
loadingOverlayProps = toNative

markProps :: Record MC.Props_Mark -> Record MC.Props_MarkImpl
markProps = toNative

menuProps :: Record MC.Props_Menu -> Record MC.Props_MenuImpl
menuProps = toNative

menuItemProps :: Record MC.Props_MenuItem -> Record MC.Props_MenuItemImpl
menuItemProps = toNative

menuTargetProps :: Record MC.Props_MenuTarget -> Record MC.Props_MenuTargetImpl
menuTargetProps = toNative

modalProps :: Record MC.Props_Modal -> Record MC.Props_ModalImpl
modalProps = toNative

multiSelectProps :: Record MC.Props_MultiSelect -> Record MC.Props_MultiSelectImpl
multiSelectProps = toNative

nativeSelectProps :: Record MC.Props_NativeSelect -> Record MC.Props_NativeSelectImpl
nativeSelectProps = toNative

navLinkProps :: Record MC.Props_NavLink -> Record MC.Props_NavLinkImpl
navLinkProps = toNative

notificationProps :: Record MC.Props_Notification -> Record MC.Props_NotificationImpl
notificationProps = toNative

numberFormatterProps :: Record MC.Props_NumberFormatter -> Record MC.Props_NumberFormatterImpl
numberFormatterProps = toNative

numberInputProps :: Record MC.Props_NumberInput -> Record MC.Props_NumberInputImpl
numberInputProps = toNative

optionalPortalProps :: Record MC.Props_OptionalPortal -> Record MC.Props_OptionalPortalImpl
optionalPortalProps = toNative

overlayProps :: Record MC.Props_Overlay -> Record MC.Props_OverlayImpl
overlayProps = toNative

paginationProps :: Record MC.Props_Pagination -> Record MC.Props_PaginationImpl
paginationProps = toNative

paperProps :: Record MC.Props_Paper -> Record MC.Props_PaperImpl
paperProps = toNative

passwordInputProps :: Record MC.Props_PasswordInput -> Record MC.Props_PasswordInputImpl
passwordInputProps = toNative

pillProps :: Record MC.Props_Pill -> Record MC.Props_PillImpl
pillProps = toNative

pillGroupProps :: Record MC.Props_PillGroup -> Record MC.Props_PillGroupImpl
pillGroupProps = toNative

pillsInputProps :: Record MC.Props_PillsInput -> Record MC.Props_PillsInputImpl
pillsInputProps = toNative

pillsInputFieldProps :: Record MC.Props_PillsInputField -> Record MC.Props_PillsInputFieldImpl
pillsInputFieldProps = toNative

pinInputProps :: Record MC.Props_PinInput -> Record MC.Props_PinInputImpl
pinInputProps = toNative

popoverProps :: Record MC.Props_Popover -> Record MC.Props_PopoverImpl
popoverProps = toNative

popoverTargetProps :: Record MC.Props_PopoverTarget -> Record MC.Props_PopoverTargetImpl
popoverTargetProps = toNative

portalProps :: Record MC.Props_Portal -> Record MC.Props_PortalImpl
portalProps = toNative

progressProps :: Record MC.Props_Progress -> Record MC.Props_ProgressImpl
progressProps = toNative

progressRootProps :: Record MC.Props_ProgressRoot -> Record MC.Props_ProgressRootImpl
progressRootProps = toNative

progressSectionProps :: Record MC.Props_ProgressSection -> Record MC.Props_ProgressSectionImpl
progressSectionProps = toNative

radioProps :: Record MC.Props_Radio -> Record MC.Props_RadioImpl
radioProps = toNative

radioGroupProps :: Record MC.Props_RadioGroup -> Record MC.Props_RadioGroupImpl
radioGroupProps = toNative

rangeSliderProps :: Record MC.Props_RangeSlider -> Record MC.Props_RangeSliderImpl
rangeSliderProps = toNative

ratingProps :: Record MC.Props_Rating -> Record MC.Props_RatingImpl
ratingProps = toNative

ringProgressProps :: Record MC.Props_RingProgress -> Record MC.Props_RingProgressImpl
ringProgressProps = toNative

scrollAreaProps :: Record MC.Props_ScrollArea -> Record MC.Props_ScrollAreaImpl
scrollAreaProps = toNative

segmentedControlProps :: Record MC.Props_SegmentedControl -> Record MC.Props_SegmentedControlImpl
segmentedControlProps = toNative

selectProps :: Record MC.Props_Select -> Record MC.Props_SelectImpl
selectProps = toNative

simpleGridProps :: Record MC.Props_SimpleGrid -> Record MC.Props_SimpleGridImpl
simpleGridProps = toNative

chipGroupProps :: Record (MC.Props_ChipGroup String) -> Record (MC.Props_ChipGroupImpl String)
chipGroupProps = toNative

skeletonProps :: Record MC.Props_Skeleton -> Record MC.Props_SkeletonImpl
skeletonProps = toNative

sliderProps :: Record MC.Props_Slider -> Record MC.Props_SliderImpl
sliderProps = toNative

spaceProps :: Record MC.Props_Space -> Record MC.Props_SpaceImpl
spaceProps = toNative

spoilerProps :: Record MC.Props_Spoiler -> Record MC.Props_SpoilerImpl
spoilerProps = toNative

stackProps :: Record MC.Props_Stack -> Record MC.Props_StackImpl
stackProps = toNative

stepperProps :: Record MC.Props_Stepper -> Record MC.Props_StepperImpl
stepperProps = toNative

stepperCompletedProps :: Record MC.Props_StepperCompleted -> Record MC.Props_StepperCompletedImpl
stepperCompletedProps = toNative

stepperStepProps :: Record MC.Props_StepperStep -> Record MC.Props_StepperStepImpl
stepperStepProps = toNative

switchProps :: Record MC.Props_Switch -> Record MC.Props_SwitchImpl
switchProps = toNative

switchGroupProps :: Record MC.Props_SwitchGroup -> Record MC.Props_SwitchGroupImpl
switchGroupProps = toNative

tabListProps :: Record MC.Props_TabList -> Record MC.Props_TabListImpl
tabListProps = toNative

tabPanelProps :: Record MC.Props_TabPanel -> Record MC.Props_TabPanelImpl
tabPanelProps = toNative

tableProps :: Record MC.Props_Table -> Record MC.Props_TableImpl
tableProps = toNative

tableCaptionProps :: Record MC.Props_TableCaption -> Record MC.Props_TableCaptionImpl
tableCaptionProps = toNative

tableScrollContainerProps :: Record MC.Props_TableScrollContainer -> Record MC.Props_TableScrollContainerImpl
tableScrollContainerProps = toNative

tableTbodyProps :: Record MC.Props_TableTbody -> Record MC.Props_TableTbodyImpl
tableTbodyProps = toNative

tableTdProps :: Record MC.Props_TableTd -> Record MC.Props_TableTdImpl
tableTdProps = toNative

tableTfootProps :: Record MC.Props_TableTfoot -> Record MC.Props_TableTfootImpl
tableTfootProps = toNative

tableThProps :: Record MC.Props_TableTh -> Record MC.Props_TableThImpl
tableThProps = toNative

tableTheadProps :: Record MC.Props_TableThead -> Record MC.Props_TableTheadImpl
tableTheadProps = toNative

tableTrProps :: Record MC.Props_TableTr -> Record MC.Props_TableTrImpl
tableTrProps = toNative

tabsProps :: Record MC.Props_Tabs -> Record MC.Props_TabsImpl
tabsProps = toNative

tabsTabProps :: Record MC.Props_TabsTab -> Record MC.Props_TabsTabImpl
tabsTabProps = toNative

tagsInputProps :: Record MC.Props_TagsInput -> Record MC.Props_TagsInputImpl
tagsInputProps = toNative

textProps :: Record MC.Props_Text -> Record MC.Props_TextImpl
textProps = toNative

textInputProps :: Record MC.Props_TextInput -> Record MC.Props_TextInputImpl
textInputProps = toNative

textareaProps :: Record MC.Props_Textarea -> Record MC.Props_TextareaImpl
textareaProps = toNative

themeIconProps :: Record MC.Props_ThemeIcon -> Record MC.Props_ThemeIconImpl
themeIconProps = toNative

timelineProps :: Record MC.Props_Timeline -> Record MC.Props_TimelineImpl
timelineProps = toNative

timelineItemProps :: Record MC.Props_TimelineItem -> Record MC.Props_TimelineItemImpl
timelineItemProps = toNative

titleProps :: Record MC.Props_Title -> Record MC.Props_TitleImpl
titleProps = toNative

tooltipProps :: Record MC.Props_Tooltip -> Record MC.Props_TooltipImpl
tooltipProps = toNative

tooltipGroupProps :: Record MC.Props_TooltipGroup -> Record MC.Props_TooltipGroupImpl
tooltipGroupProps = toNative

transitionProps :: Record MC.Props_Transition -> Record MC.Props_TransitionImpl
transitionProps = toNative

unstyledButtonProps :: Record MC.Props_UnstyledButton -> Record MC.Props_UnstyledButtonImpl
unstyledButtonProps = toNative
