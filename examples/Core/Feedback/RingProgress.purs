module Examples.Core.Feedback.RingProgress where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Icons (icon)
import React.Icons.Tb (tbCheck)

-- Usage
-- https://mantine.dev/core/ring-progress/#usage
usage :: JSX
usage =
  MC.ringProgress
    { label: MC.text { size: MC.Preset MC.ExtraSmall, ta: MC.TextAlignCenter
                     , children: [ DOM.text "Application data usage" ]
                     }
    , sections:
        [ MC.partial { color: MC.Cyan,   value: 40.0 }
        , MC.partial { color: MC.Orange, value: 15.0 }
        , MC.partial { color: MC.Grape,  value: 15.0 }
        ]
    }

-- Size, thickness & rounded caps
-- https://mantine.dev/core/ring-progress/#size-thickness--rounded-caps
sizeThicknessRoundedCaps :: Component StylingProps
sizeThicknessRoundedCaps = component "RingProgress_styling" \{ size, thickness, roundCaps } ->
  pure $
    MC.ringProgress
      { size, thickness, roundCaps
      , sections:
           [ MC.partial { value: 40.0, color: MC.Cyan   }
           , MC.partial { value: 15.0, color: MC.Orange }
           , MC.partial { value: 15.0, color: MC.Grape  }
           ]
      }

type StylingProps =
  { size      :: MC.Pixels
  , thickness :: MC.Pixels
  , roundCaps :: Boolean
  }

-- Sections tooltips
-- https://mantine.dev/core/ring-progress/#sections-tooltips
sectionsTooltips :: JSX
sectionsTooltips =
  MC.ringProgress
    { size: 170.0
    , thickness: 16.0
    , label: MC.text { size: MC.Preset MC.ExtraSmall
                     , ta: MC.TextAlignCenter
                     , px: MC.Preset MC.ExtraSmall
                     , style: DOM.css { pointerEvents: "none" }
                     }
    , sections:
         [ MC.partial { value: 40.0, color: MC.Cyan,   tooltip: DOM.text "Documents – 40 Gb" }
         , MC.partial { value: 15.0, color: MC.Orange, tooltip: DOM.text "Apps – 25 Gb"      }
         , MC.partial { value: 15.0, color: MC.Grape,  tooltip: DOM.text "Other – 15 Gb"     }
         ]
    }

-- Root color
-- https://mantine.dev/core/ring-progress/#root-color
rootColor :: JSX
rootColor =
  MC.ringProgress
    { rootColor: MC.Red
    , sections:
         [ MC.partial { value: 40.0, color: MC.Yellow }
         ]
    }

-- Sections props
-- https://mantine.dev/core/ring-progress/#sections-props
sectionProps :: Component Unit
sectionProps = component "RingProgress_sectionProps" \_ -> React.do
  hovered /\ setHovered <- React.useState' Nothing
  let reset = handler_ (setHovered Nothing)
      hover = handler_ <<< setHovered <<< Just
      legend = "Hovered section: " <> maybe "none" show hovered
  pure $ fold
    [ MC.ringProgress
        { onMouseLeave: reset
        , sections:
            [ MC.partial { value: 40.0, color: MC.Cyan,   onMouseEnter: hover 1, onMouseLeave: reset }
            , MC.partial { value: 15.0, color: MC.Orange, onMouseEnter: hover 2, onMouseLeave: reset }
            , MC.partial { value: 15.0, color: MC.Grape,  onMouseEnter: hover 3, onMouseLeave: reset }
            ]
        }
    , MC.text_ [ DOM.text legend ]
    ]

-- Customize label
-- https://mantine.dev/core/ring-progress/#customize-label
customizeLabel :: JSX
customizeLabel =
  fold
    [ MC.ringProgress
        { sections: [ MC.partial { value: 40.0, color: MC.Blue } ]
        , label:
            MC.text
              { c: MC.Blue
              , fw: MC.FontWeight 700
              , ta: MC.TextAlignCenter
              , size: MC.Preset MC.ExtraLarge
              , children: [ DOM.text "40%" ]
              }
        }
    , MC.ringProgress
        { sections: [ MC.partial { value: 100.0, color: MC.Teal } ]
        , label:
            MC.center_
              [ MC.actionIcon
                  { color: MC.Teal
                  , children: [ icon tbCheck { style: DOM.css { width: "rem(22)", height: "rem(22)" } } ]
                  , variant: MC.ActionIconLight
                  , radius: MC.Preset MC.ExtraLarge
                  , size: MC.Preset MC.ExtraLarge
                  }
              ]
        }
    ]
