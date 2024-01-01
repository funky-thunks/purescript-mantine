module Examples.Core.Inputs.Textarea where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Basic.Hooks as React

-- Usage
-- https://mantine.dev/core/textarea/#usage
usage :: Usage -> JSX
usage = MC.textarea

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

-- Autosize
-- https://mantine.dev/core/textarea/#autosize
autosize :: JSX
autosize =
  fold
    [ MC.textarea
        { autosize: true
        , minRows: 2
        , label: DOM.text "Autosize with no rows limit"
        , placeholder:    "Autosize with no rows limit"
        }
    , MC.textarea
        { autosize: true
        , minRows: 2
        , maxRows: 4
        , label: DOM.text "Autosize with 4 rows max"
        , placeholder:    "Autosize with 4 rows max"
        }
    ]

-- Controlled
-- https://mantine.dev/core/textarea/#controlled
controlled :: Component Unit
controlled = React.component "Textarea_Controlled" \_ -> React.do
  value /\ onChange <- React.useState' ""
  pure $
    MC.textarea { value: pure value, onChange: MC.InputTargetHandler onChange }

-- Error state
-- https://mantine.dev/core/textarea/#error-state
-- Note that the "boolean variant" is not supported.
errorState :: JSX
errorState =
  MC.textarea { mt: MC.Preset MC.Medium
              , label: DOM.text "With error message"
              , placeholder:    "With error message"
              , error: DOM.text "Invalid name"
              }

-- Disabled state
-- https://mantine.dev/core/textarea/#disabled-state
disabledState :: JSX
disabledState = MC.textarea { disabled: true }

-- Styles API
-- https://mantine.dev/core/textarea/#styles-api
-- Possible via the className attribute assuming you're bundling it properly.
-- This example doesn't support bundling and CSS modules.

-- Get element ref
-- https://mantine.dev/core/textarea/#get-element-ref
getElementRef :: Component Unit
getElementRef = React.component "Textarea_GetElementRef" \_ -> React.do
  ref <- React.useRef null
  pure $ MC.textarea { ref }

-- Accessibility
-- https://mantine.dev/core/textarea/#accessibility
ariaLabel :: JSX
ariaLabel = MC.textarea { "aria-label": "My input" }
