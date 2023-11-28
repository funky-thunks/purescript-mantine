module Examples.Core.Feedback.Progress where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM

-- Usage
-- https://mantine.dev/core/progress/#usage
usage :: UsageProps -> JSX
usage { color, radius, size, value, striped, animated } =
  MC.progress { color, animated, striped, value
              , radius: MC.Preset radius, size: MC.Preset size
              }

type UsageProps =
  { color    :: MC.MantineColor
  , radius   :: MC.MantineSize
  , size     :: MC.MantineSize
  , value    :: Number
  , striped  :: Boolean
  , animated :: Boolean
  }

-- Compound components
-- https://mantine.dev/core/progress/#compound-components
compoundComponents :: JSX
compoundComponents =
  MC.progressRoot
    { size: MC.Preset MC.ExtraLarge
    , children:
        [ MC.progressSection { value: 35.0, color: MC.Cyan
                             , children: [ MC.progressLabel_ [ DOM.text "Documents" ] ]
                             }
        , MC.progressSection { value: 28.0, color: MC.Pink
                             , children: [ MC.progressLabel_ [ DOM.text "Photos" ] ]
                             }
        , MC.progressSection { value: 15.0, color: MC.Orange
                             , children: [ MC.progressLabel_ [ DOM.text "Other" ] ]
                             }
        ]
    }

-- Styles API
-- https://mantine.dev/core/progress/#styles-api
-- Possible via the className attribute assuming you're bundling it properly.
-- This example doesn't support bundling and CSS modules.
