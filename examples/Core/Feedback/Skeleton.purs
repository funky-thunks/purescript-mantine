module Examples.Core.Feedback.Skeleton where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM
import React.Basic.Hooks as React

-- Usage
-- https://mantine.dev/core/skeleton/#usage
usage :: JSX
usage =
  fold
    [ MC.skeleton { height: MC.DimensionInPixels 50.0
                  , circle: true
                  , mb: MC.Preset MC.ExtraLarge
                  }
    , MC.skeleton { height: MC.DimensionInPixels 50.0
                  , radius: MC.Preset MC.ExtraLarge
                  }
    , MC.skeleton { height: MC.DimensionInPixels 50.0
                  , radius: MC.Preset MC.ExtraLarge
                  , mt: MC.SizeInPixels 16.0
                  }
    , MC.skeleton { height: MC.DimensionInPixels 50.0
                  , width: MC.Dimension "70%"
                  , radius: MC.Preset MC.ExtraLarge
                  , mt: MC.SizeInPixels 16.0
                  }
    ]

-- With content
-- https://mantine.dev/core/skeleton/#with-content
withContent :: Component Unit
withContent = component "Skeleton_withContent" \_ -> React.do
  loading /\ setLoading <- React.useState' true
  pure $
    fold
      [ MC.skeleton { visible: loading
                    , children:
                      [ DOM.p_ [ DOM.text "Lorem ipsum dolor sit amet consectetur adipisicing elit. Modi dolor nihil amet tempore magnam optio, numquam nostrum inventore tempora assumenda saepe, aut repellat. Temporibus aspernatur aperiam magnam debitis facere odio?" ]
                      , DOM.p_ [ DOM.text "Laborum fuga quam voluptas aut pariatur delectus repudiandae commodi tempora debitis dolores vero cumque magni cum, deserunt, ad tempore consectetur libero molestias similique nemo eum! Dolore maxime voluptate inventore atque." ]
                      ]
                    }
      , MC.button { onClick: handler_ (setLoading (not loading))
                  , children: [ DOM.text "Toggle Skeleton" ]
                  }
      ]
