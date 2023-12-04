module Examples.Core.Feedback.Notification where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Icons (icon)
import React.Icons.Tb (tbCheck, tbX)

-- Usage
-- https://mantine.dev/core/notification/#usage
usage :: UsageProps -> JSX
usage { color, loading, withCloseButton, withBorder, radius, title, content } =
  MC.notification { color, loading, withCloseButton, withBorder
                  , radius: MC.Preset radius
                  , title: DOM.text title
                  , children: [ DOM.text content ]
                  }

type UsageProps =
  { color           :: MC.MantineColor
  , loading         :: Boolean
  , withCloseButton :: Boolean
  , withBorder      :: Boolean
  , radius          :: MC.MantineSize
  , title           :: String
  , content         :: String
  }

-- With icon
-- https://mantine.dev/core/notification/#with-icon
withIcon :: JSX
withIcon =
  let mkIcon i = icon i { style: DOM.css { width: "rem(20)", height: "rem(20)" }}
   in fold
        [ MC.notification { icon: mkIcon tbX,     color: MC.Red,  title: DOM.text "Bummer!" }
        , MC.notification { icon: mkIcon tbCheck, color: MC.Teal, title: DOM.text "All good!"
                          , mt: MC.Preset MC.Medium
                          }
        ]

-- Styles API
-- https://mantine.dev/core/notification/#styles-api
-- Possible via the className attribute assuming you're bundling it properly.
-- This example doesn't support bundling and CSS modules.

-- Accessibility
-- https://mantine.dev/core/notification/#accessibility
accessibility :: JSX
accessibility =
  MC.notification { closeButtonProps: MC.partial { "aria-label": "Hide notification" } }
