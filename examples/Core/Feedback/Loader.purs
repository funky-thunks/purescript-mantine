module Examples.Core.Feedback.Loader where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Basic.Hooks as React

-- Usage
-- https://mantine.dev/core/loader/#usage
usage :: Component UsageProps
usage = component "Loader_usage" \props -> do
  pure $
    MC.loader { color: props.color
              , size: MC.Preset props.size
              , type: props.type
              }

type UsageProps =
  { color :: MC.MantineColor
  , size  :: MC.MantineSize
  , type  :: MC.LoaderType
  }

-- Size prop
-- https://mantine.dev/core/loader/#size-prop
sizeProp :: JSX
sizeProp = MC.loader { size: MC.SizeInPixels 30.0 }

-- Custom CSS only loader
-- https://mantine.dev/core/loader/#custom-css-only-loader
-- This isn't supported in the bindings, this should be done via FFI.

-- Custom SVG loader
-- https://mantine.dev/core/loader/#custom-svg-loader
-- This isn't supported in the bindings, this should be done via FFI.

-- children prop
-- https://mantine.dev/core/loader/#children-prop
childrenProp :: Component Unit
childrenProp = component "Loader_childrenProp" \_ -> React.do
  visible /\ setVisible <- React.useState true
  let toggle = setVisible not
  pure $ fold
    [ MC.box { pos: MC.PositionRelative
             , children:
                 [ MC.loadingOverlay { visible, loaderProps: MC.partial { children: DOM.text "Loading..." } }
                 -- other content
                 ]
             }
    , MC.group { justify: MC.JustifyContentCenter
               , children:
                   [ MC.button { onClick: handler_ toggle
                               , children: [ DOM.text "Toggle overlay" ]
                               }
                   ]
               }
    ]
