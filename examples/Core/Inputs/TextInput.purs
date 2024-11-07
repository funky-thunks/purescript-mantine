module Examples.Core.Inputs.TextInput where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Icons (icon)
import React.Icons.Tb (tbAt)

-- Usage
-- https://mantine.dev/core/text-input/#usage
usage :: Usage -> JSX
usage = MC.textInput

type Usage =
  { variant      :: MC.InputVariant
  , size         :: MC.MantineSize
  , radius       :: MC.MantineNumberSize
  , label        :: JSX
  , withAsterisk :: Boolean
  , description  :: JSX
  , error        :: JSX
  , placeholder  :: String
  }

-- Controlled
-- https://mantine.dev/core/text-input/#controlled
controlled :: Component Unit
controlled = React.component "TextInput_Controlled" \_ -> React.do
  value /\ onChange <- React.useState' ""
  pure $
    MC.textInput { value: pure value, onChange: MC.InputTargetHandler onChange }

-- Left and right sections
-- https://mantine.dev/core/text-input/#left-and-right-sections
leftAndRightSections :: JSX
leftAndRightSections =
  let iconAt = icon tbAt { style: DOM.css { width: "rem(16)", height: "rem(16)" } }
   in fold
        [ MC.textInput { leftSectionPointerEvents: MC.PointerEventsNone
                       , leftSection: iconAt
                       , label: DOM.text "Your email"
                       , placeholder:    "Your email"
                       }
        , MC.textInput { rightSectionPointerEvents: MC.PointerEventsNone
                       , rightSection: iconAt
                       , mt: MC.Preset MC.Medium
                       , label: DOM.text "Your email"
                       , placeholder:    "Your email"
                       }
        ]

-- Error state
-- https://mantine.dev/core/text-input/#error-state
-- Note that the "boolean variant" is not supported.
errorState :: JSX
errorState =
  MC.textInput { mt: MC.Preset MC.Medium
               , label: DOM.text "With error message"
               , placeholder:    "With error message"
               , error: DOM.text "Invalid name"
               }

-- Disabled state
-- https://mantine.dev/core/text-input/#disabled-state
disabledState :: JSX
disabledState =
  MC.textInput { disabled: true
               , label: DOM.text "Disabled input"
               , placeholder:    "Disabled input"
               }

-- Styles API
-- https://mantine.dev/core/text-input/#styles-api
-- Possible via the className attribute assuming you're bundling it properly.
-- This example doesn't support bundling and CSS modules.

-- Get element ref
-- https://mantine.dev/core/text-input/#get-element-ref
getElementRef :: Component Unit
getElementRef = React.component "TextInput_GetElementRef" \_ -> React.do
  ref <- React.useRef null
  pure $ MC.textInput { ref }

-- Accessibility
-- https://mantine.dev/core/text-input/#accessibility
ariaLabel :: JSX
ariaLabel = MC.textInput { "aria-label": "My input" }
