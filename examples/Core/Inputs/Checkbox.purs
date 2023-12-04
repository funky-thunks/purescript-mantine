module Examples.Core.Inputs.Checkbox where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Icons (icon_)
import React.Icons.Tb (tbBiohazard, tbRadioactive)

-- Usage
-- https://mantine.dev/core/checkbox/#usage
usage :: Component UsageProps
usage = component "Checkbox_usage" \{ labelPosition, label, description, error, color, radius, size, disabled, indeterminate } ->
  pure $
    MC.checkbox
      { label:       DOM.text label
      , description: DOM.text description
      , error:       DOM.text error
      , radius:      MC.Preset radius
      , labelPosition, color, size, disabled, indeterminate
      }

type UsageProps =
  { labelPosition :: MC.CheckableLabelPosition
  , label         :: String
  , description   :: String
  , error         :: String
  , color         :: MC.MantineColor
  , radius        :: MC.MantineSize
  , size          :: MC.MantineSize
  , disabled      :: Boolean
  , indeterminate :: Boolean
  }

-- States
-- https://mantine.dev/core/checkbox/#states
states :: JSX
states =
  fold
    [ MC.checkbox { label: DOM.text "Default checkbox" }
    , MC.checkbox { label: DOM.text "Indeterminate checkbox", indeterminate: true }
    , MC.checkbox { label: DOM.text "Indeterminate checked checkbox", checked: true, indeterminate: true }
    , MC.checkbox { label: DOM.text "Checked checkbox", checked: true }
    , MC.checkbox { label: DOM.text "Disabled checkbox", disabled: true }
    , MC.checkbox { label: DOM.text "Disabled indeterminate checkbox", disabled: true, indeterminate: true }
    , MC.checkbox { label: DOM.text "Disabled checked checkbox", disabled: true, checked: true }
    ]

-- Change icons
-- https://mantine.dev/core/checkbox/#change-icons
changeIcons :: JSX
changeIcons =
  let checkboxIcon { indeterminate } =
        if indeterminate
        then icon_ tbRadioactive
        else icon_ tbBiohazard
   in fold
        [ MC.checkbox { icon: checkboxIcon
                      , label: DOM.text "Custom icon"
                      , defaultChecked: true
                      }
        , MC.checkbox { icon: checkboxIcon
                      , label: DOM.text "Custom icon: indeterminate"
                      , indeterminate: true
                      , mt: MC.Preset MC.Small
                      }
        ]

-- Change icon color
-- https://mantine.dev/core/checkbox/#change-icon-color
changeIconColor :: JSX
changeIconColor =
  MC.checkbox { label: DOM.text "Bright lime checkbox"
              , defaultChecked: true
              , color: MC.Lime4
              , iconColor: MC.Dark8
              , size: MC.Medium
              }
