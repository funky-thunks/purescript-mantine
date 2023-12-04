module Examples.Core.Feedback.Alert where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM

-- Usage
-- https://mantine.dev/core/alert/#usage
usage :: Component UsageProps
usage = component "Alert_usage" \{ variant, color, radius, withCloseButton, title, message } ->
  pure $
    MC.alert
      { variant, color, withCloseButton
      , radius:   MC.Preset radius
      , title:    DOM.text title
      , children: DOM.text message
      }

usageExample :: UsageProps
usageExample =
  { variant: MC.AlertVariantLight
  , color: MC.Blue
  , radius: MC.Small
  , withCloseButton: false
  , title: "Alert title"
  , message: "Lorem ipsum dolor sit, amet consectetur adipisicing elit. At officiis, quae tempore necessitatibus placeat saepe."
  }

type UsageProps =
  { variant         :: MC.AlertVariant
  , color           :: MC.MantineColor
  , radius          :: MC.MantineSize
  , withCloseButton :: Boolean
  , title           :: String
  , message         :: String
  }

-- Styles API
-- https://mantine.dev/core/alert/#styles-api
-- Possible via the className attribute assuming you're bundling it properly.
-- This example doesn't support bundling and CSS modules.

-- Accessibility
-- https://mantine.dev/core/alert/#accessibility
accessibility :: JSX
accessibility = MC.alert { withCloseButton: true, closeButtonLabel: "Dismiss" }
