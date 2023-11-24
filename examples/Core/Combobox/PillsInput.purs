module Examples.Core.Combobox.PillsInput where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import Web.Event.Event as WE
import Web.UIEvent.KeyboardEvent as KE

pillsInputUsage :: JSX
pillsInputUsage =
  MC.pillsInput
    { label: DOM.text "PillsInput"
    , children:
        [ MC.pillGroup_
            [ MC.pill_ "React"
            , MC.pill_ "Vue"
            , MC.pill_ "Svelte"
            , MC.pillsInputField { placeholder: "Enter tags" }
            ]
        ]
    }

pillsInputInputProps :: JSX
pillsInputInputProps =
  MC.pillsInput
    { label: DOM.text "Input label"
    , description : DOM.text "Input description"
    , variant: MC.InputVariantFilled
    , radius: MC.Preset MC.Medium
    , size: MC.Medium
    , children:
        [ MC.pillGroup_
            [ MC.pill_ "React"
            , MC.pill_ "Vue"
            , MC.pill_ "Svelte"
            , MC.pillsInputField { placeholder: "Enter tags" }
            ]
        ]
    }

pillsInputSearchableExample :: Component Unit
pillsInputSearchableExample =
    let groceries = ["🍎 Apples", "🍌 Bananas", "🥦 Broccoli", "🥕 Carrots", "🍫 Chocolate"]
     in component "PillsInputSearchableExample" \_ -> React.do
          store <- MC.useCombobox {}
          search /\ setSearch <- React.useState' Nothing
          value  /\ setValue  <- React.useState []
          let handleValueSelect val =
                setValue \ current -> if val `elem` current
                                      then filter (\v -> v /= val) current
                                      else current <> [val]
              handleValueRemove val = setValue (filter (\v -> v /= val))
              mkPill item =
                MC.pill
                  { key: item
                  , withRemoveButton: true
                  , onRemove: handleValueRemove item
                  , children: [ DOM.text item ]
                  }
              pills = mkPill <$> value

              options = MC.comboboxOptions_ (
                mkOption <$> filter (\item -> Just item == search) groceries
              )
              mkOption item =
                let active = item `elem` value
                 in MC.comboboxOption
                      { key:   item
                      , value: item
                      , active
                      , children:
                          [ MC.group
                              { gap: MC.Preset MC.Small
                              , children:
                                  [ guard active (MC.checkIcon { size: MC.SizeInPixels 12.0 } )
                                  , DOM.span_ [ DOM.text item ]
                                  ]
                              }
                          ]
                      }
              pillsInputField =
                MC.pillsInputField
                  { onFocus: handler_ (store.openDropdown  MC.ComboboxDropdownEventSourceUnknown)
                  , onBlur:  handler_ (store.closeDropdown MC.ComboboxDropdownEventSourceUnknown)
                  , value: search
                  , placeholder: "Search values"
                  , onChange: MC.InputCurrentTargetHandler $ \v -> do
                      store.updateSelectedOptionIndex Nothing
                      setSearch (Just v)
                  , onKeyDown: handler nativeEvent $ \e ->
                      let isBackspace = (KE.key <$> KE.fromEvent e) == Just "Baskspace"
                          isSearchEmpty = maybe 0 length search == 0
                       in guard (isBackspace && isSearchEmpty) do
                            WE.preventDefault e
                            foldMap handleValueRemove (last value)
                  }
              comboboxTarget = MC.comboboxEventsTarget_ [ pillsInputField ]
              pillsInput =
                MC.pillsInput
                  { onClick: handler_ (store.openDropdown MC.ComboboxDropdownEventSourceMouse)
                  , children:
                      [ MC.pillGroup_ (pills <> pure comboboxTarget)
                      ]
                  }
          pure $
            MC.combobox
              { store
              , onOptionSubmit: \v _ -> handleValueSelect v
              , children:
                  [ MC.comboboxDropdownTarget_ [ pillsInput ]
                  , MC.comboboxDropdown_       [ options    ]
                  ]
              }
