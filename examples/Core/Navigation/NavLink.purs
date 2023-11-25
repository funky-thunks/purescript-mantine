module Examples.Core.Navigation.NavLink where

import Data.Array (mapWithIndex)
import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Icons (icon, icon_)
import React.Icons.Tb (tbActivity, tbChevronRight, tbCircleOff, tbFingerprint, tbGauge, tbHome2)
import Record (union)

-- Usage
-- https://mantine.dev/core/nav-link/#usage
usage :: JSX
usage =
  let leftIcon  i = icon i { size: "1   rem", style: DOM.css { stroke: 1.5 } }
      rightIcon i = icon i { size: "0.8 rem", style: DOM.css { stroke: 1.5 } }
   in fold
        [ MC.navLink { label: DOM.text "With icon"
                     , leftSection: leftIcon tbGauge
                     }
        , MC.navLink { label: DOM.text "With right section"
                     , leftSection:  leftIcon  tbHome2
                     , rightSection: rightIcon tbChevronRight
                     }
        , MC.navLink { label: DOM.text "Disabled"
                     , leftSection: leftIcon tbCircleOff
                     , disabled: true
                     }
        , MC.navLink { label: DOM.text "With description"
                     , description: DOM.text "Additional information"
                     , leftSection:
                         MC.badge
                           { size: MC.ExtraSmall
                           , variant: MC.BadgeVariantFilled
                           , color: MC.Red
                           , w: MC.SizeInPixels 16.0
                           , h: MC.SizeInPixels 16.0
                           , p: MC.SizeInPixels  0.0
                           , children: [ DOM.text "3" ]
                           }
                     }
        , MC.navLink { label: DOM.text "Active subtle"
                     , leftSection:  leftIcon  tbActivity
                     , rightSection: rightIcon tbChevronRight
                     , active: true
                     , variant: MC.NavLinkSubtle
                     }
        , MC.navLink { label: DOM.text "Active light"
                     , leftSection:  leftIcon  tbActivity
                     , rightSection: rightIcon tbChevronRight
                     , active: true
                     }
        , MC.navLink { label: DOM.text "Active filled"
                     , leftSection:  leftIcon  tbActivity
                     , rightSection: rightIcon tbChevronRight
                     , active: true
                     , variant: MC.NavLinkFilled
                     }
        ]

-- Active
-- https://mantine.dev/core/nav-link/#active
active :: Component { color :: MC.MantineColor, variant :: MC.NavLinkVariant }
active = component "NavLinkActive" \ { color, variant } -> React.do
  activeIndex /\ setActive <- React.useState' 0
  let items =
        [ { leftSection: icon_ tbGauge
          , label: "Dashboard"
          , description: Just $ DOM.text "Item with description"
          , rightSection: Nothing
          }
        , { leftSection: icon_ tbFingerprint
          , label: "Security"
          , description: Nothing
          , rightSection: Just $ icon tbChevronRight { size: "1 rem", style: DOM.css { stroke: 1.5 } }
          }
        , { leftSection: icon_ tbActivity
          , label: "Activity"
          , description: Nothing
          , rightSection: Nothing
          }
        ]
      mkItem index { label, description, leftSection, rightSection } =
        let base =
              { key: label
              , active: index == activeIndex
              , label: DOM.text label
              , leftSection
              , onClick: handler_ (setActive index)
              , color
              , variant
              }
         in case description, rightSection of
              Just d,  Nothing -> MC.navLink $ base `union` { description: d }
              Just d,  Just rs -> MC.navLink $ base `union` { description: d, rightSection: rs }
              Nothing, Just rs -> MC.navLink $ base `union` { rightSection: rs }
              Nothing, Nothing -> MC.navLink base
  pure $ MC.box { w: MC.SizeInPixels 220.0
                , children: mapWithIndex mkItem items
                }

-- Nested navlinks
-- https://mantine.dev/core/nav-link/#nested-navlinks
nestedNavlinks :: JSX
nestedNavlinks =
  let mkIcon i = icon i { size: "1 rem", style: DOM.css { stroke: 1.5 } }
   in fold
        [ MC.navLink { label: DOM.text "First parent link", childrenOffset: MC.SizeInPixels 28.0
                     , leftSection: mkIcon tbGauge
                     , children:
                         [ MC.navLink { label: DOM.text "First child link"  }
                         , MC.navLink { label: DOM.text "Second child link" }
                         , MC.navLink { label: DOM.text "Nested parent link"
                                      , childrenOffset: MC.SizeInPixels 28.0
                                      , children:
                                         [ MC.navLink { label: DOM.text "First child link"  }
                                         , MC.navLink { label: DOM.text "Second child link" }
                                         , MC.navLink { label: DOM.text "Third child link"  }
                                         ]
                                      }
                         ]
                     }
        , MC.navLink { label: DOM.text "Second parent link", childrenOffset: MC.SizeInPixels 28.0, defaultOpened: true
                     , leftSection: mkIcon tbFingerprint
                     , children:
                         [ MC.navLink { label: DOM.text "First child link"  }
                         , MC.navLink { label: DOM.text "Second child link" }
                         , MC.navLink { label: DOM.text "Third child link"  }
                         ]
                     }
        ]

-- Polymorphic component
-- https://mantine.dev/core/nav-link/#polymorphic-component
--
-- Custom components passing isn't supported. You should handle it in the FFI (feel free to reuse `navLink`'s' implementation).
polymorphicComponent :: JSX
polymorphicComponent = MC.navLink { component: "button" }

-- Get element ref
-- https://mantine.dev/core/nav-link/#get-element-ref
getElementRef :: Component Unit
getElementRef = React.component "NavLink_GetElementRef" \_ -> React.do
  ref <- React.useRef null
  pure $ MC.navLink { ref }
