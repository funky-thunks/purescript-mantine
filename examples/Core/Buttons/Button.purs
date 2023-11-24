module Examples.Core.Buttons.Button where

import Examples.Core.Prelude
import Mantine.Core as MC
import Mantine.FFI (toNative)
import React.Basic.DOM as DOM
import React.Icons (icon)
import React.Icons.Tb (tbArrowRight, tbDownload, tbPhoto)

-- Usage
-- https://mantine.dev/core/button/#usage
usage :: Usage -> JSX
usage { variant, color, size, radius } =
  MC.button { variant, color, size: MC.Padded size, radius, children: [ DOM.text "Button" ] }

type Usage =
  { variant :: MC.ButtonVariant
  , color   :: MC.MantineColor
  , size    :: MC.MantineSize
  , radius  :: MC.Radius
  }

-- Full width
-- https://mantine.dev/core/button/#full-width
fullWidth :: JSX
fullWidth =
  MC.button { fullWidth: true, children: [ DOM.text "Full width button" ] }

-- Left and right sections
-- https://mantine.dev/core/button/#left-and-right-sections
leftAndRightSections :: JSX
leftAndRightSections =
  let photoIcon      = icon tbPhoto      { size: "14px" }
      downloadIcon   = icon tbDownload   { size: "14px" }
      arrowRightIcon = icon tbArrowRight { size: "14px" }
   in MC.group
        { justify: MC.JustifyContentCenter
        , children:
            [ MC.button { leftSection: photoIcon
                        , variant: MC.ButtonVariantDefault
                        , children: [ DOM.text "Gallery" ]
                        }
            , MC.button { rightSection: downloadIcon
                        , children: [ DOM.text "Download" ]
                        }
            , MC.button { leftSection: photoIcon
                        , rightSection: arrowRightIcon
                        , variant: MC.ButtonVariantLight
                        , children: [ DOM.text "Visit gallery" ]
                        }
            ]
        }

-- Sections position
-- https://mantine.dev/core/button/#sections-position
sectionsPosition :: SectionsPosition -> JSX
sectionsPosition { justify } =
  let photoIcon = icon tbPhoto { size: "16px" }
   in fold
        [ MC.button { justify
                    , fullWidth: true
                    , leftSection: photoIcon
                    , rightSection: photoIcon
                    , variant: MC.ButtonVariantDefault
                    , children: [ DOM.text "Button label" ]
                    }
        , MC.button { justify
                    , fullWidth: true
                    , leftSection: photoIcon
                    , variant: MC.ButtonVariantDefault
                    , mt: MC.Medium
                    , children: [ DOM.text "Button label" ]
                    }
        , MC.button { justify
                    , fullWidth: true
                    , rightSection: photoIcon
                    , variant: MC.ButtonVariantDefault
                    , mt: MC.Medium
                    , children: [ DOM.text "Button label" ]
                    }
        , MC.button { justify
                    , fullWidth: true
                    , leftSection: DOM.span_ []
                    , rightSection: photoIcon
                    , variant: MC.ButtonVariantDefault
                    , mt: MC.Medium
                    , children: [ DOM.text "Button label" ]
                    }
        ]

type SectionsPosition =
  { justify :: MC.JustifyContent
  }

-- Compact size
-- https://mantine.dev/core/button/#compact-size
compactSize :: MC.MantineSize -> JSX
compactSize baseSize =
  fold
    [ MC.button { size: MC.Padded baseSize
                , children: [ DOM.text ("Regular " <> toNative baseSize) ]
                }
    , MC.button { size: MC.Compact baseSize
                , children: [ DOM.text ("Compact " <> toNative baseSize) ]
                }
    ]

-- Gradient variant
-- https://mantine.dev/core/button/#gradient-variant
gradient :: MC.MantineColor -> MC.MantineColor -> MC.Degrees -> JSX
gradient from to angle =
  MC.button { gradient: { from, to, angle: pure angle }
            , variant: MC.ButtonVariantGradient
            , children: [ DOM.text "Gradient button" ]
            }

-- Disabled state
-- https://mantine.dev/core/button/#disabled-state
disabledState :: JSX
disabledState =
  MC.button { disabled: true
            , children: [ DOM.text "Disabled button" ]
            }

-- Disabled state when Button is link
-- https://mantine.dev/core/button/#disabled-state-when-button-is-link
disabledStateWhenButtonIsLink :: JSX
disabledStateWhenButtonIsLink =
  MC.button { component: "a"
            , href: "https://mantine.dev"
            , "data-disabled": true
            , onClick: handler preventDefault (const (pure unit))
            , children: [ DOM.text "Disabled link" ]
            }

-- Customize disabled styles
-- https://mantine.dev/core/button/#customize-disabled-styles
-- Possible via the className attribute assuming you're bundling it properly.
-- This example doesn't support bundling and CSS modules.

-- Disabled button with tooltip
-- https://mantine.dev/core/button/#disabled-button-with-tooltip
disabledButtonWithTooltip :: JSX
disabledButtonWithTooltip =
  MC.tooltip { label: DOM.text "Tooltip for disabled button"
             , children:
                 [ MC.button { "data-disabled": true
                             , onClick: handler preventDefault (const (pure unit))
                             , children: [ DOM.text "Disabled button with tooltip" ]
                             }
                 ]
             }

-- Loading state
-- https://mantine.dev/core/button/#loading-state
loadingState :: JSX
loadingState =
  MC.group_
    [ MC.button { loading: true, children: [ DOM.text "Filled button" ] }
    , MC.button { loading: true, variant: MC.ButtonVariantLight,   children: [ DOM.text "Light button"   ] }
    , MC.button { loading: true, variant: MC.ButtonVariantOutline, children: [ DOM.text "Outline button" ] }
    ]

-- Loader props
-- https://mantine.dev/core/button/#loader-props
loaderProps :: JSX
loaderProps =
  MC.button { loading: true
            , loaderProps: MC.partial { type: MC.LoaderTypeDots }
            , children: [ DOM.text "Loading button" ]
            }

-- Styles API
-- https://mantine.dev/core/button/#styles-api
-- Possible via the className attribute assuming you're bundling it properly.
-- This example doesn't support bundling and CSS modules.

-- Custom variants
-- https://mantine.dev/core/button/#custom-variants
-- Note that this example is incomplete and requires you to customize the theme passed to the MantineProvider.
-- It's probably better to do it directly in javascript like the original example from the official documentation.
customVariants :: JSX
customVariants =
  MC.group_
    [ MC.button { variant: MC.ButtonVariantCustom "danger",  children: [ DOM.text "Danger variant"  ] }
    , MC.button { variant: MC.ButtonVariantCustom "primary", children: [ DOM.text "Primary variant" ] }
    ]

-- Customize variants colors
-- https://mantine.dev/core/button/#customize-variants-colors
-- Note that this example is incomplete and requires you to customize the theme passed to the MantineProvider.
-- It's probably better to do it directly in javascript like the original example from the official documentation.
customizeVariantsColors :: JSX
customizeVariantsColors =
  MC.group_
    [ MC.button { variant: MC.ButtonVariantFilled,          children: [ DOM.text "Lime filled button"  ], color: MC.Lime4  }
    , MC.button { variant: MC.ButtonVariantLight,           children: [ DOM.text "Orange light button" ], color: MC.Orange }
    , MC.button { variant: MC.ButtonVariantCustom "danger", children: [ DOM.text "Danger button"       ] }
    ]

-- Button.Group
-- https://mantine.dev/core/button/#buttongroup
buttonGroup :: MC.Orientation -> JSX
buttonGroup orientation =
  MC.buttonGroup
    { orientation
    , children:
        [ MC.button { variant: MC.ButtonVariantDefault, children: [ DOM.text "First"  ] }
        , MC.button { variant: MC.ButtonVariantDefault, children: [ DOM.text "Second" ] }
        , MC.button { variant: MC.ButtonVariantDefault, children: [ DOM.text "Third"  ] }
        ]
    }
