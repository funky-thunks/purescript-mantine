module Examples.Core.Layout.AspectRatio where

import Examples.Core.Prelude
import Mantine.Core as MC
import React.Basic.DOM as DOM

-- Usage
-- https://mantine.dev/core/aspect-ratio/#usage
usage :: JSX
usage =
  MC.aspectRatio
    { ratio: 720.0 / 1080.0
    , maw: MC.SizeInPixels 300.0
    -- , mx: "auto"
    , children:
        [ DOM.img { src: "https://images.unsplash.com/photo-1527118732049-c88155f2107c?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=720&q=80"
                  , alt: "Panda"
                  }
        ]
    }

-- Inside flex container
-- https://mantine.dev/core/aspect-ratio/#inside-flex-container
insideFlexContainer :: JSX
insideFlexContainer =
  DOM.div
    { style: DOM.css { display: "flex" }
    , children:
      [ MC.aspectRatio
          { ratio: 1.0
          , style: DOM.css { flex: "0 0 rem(100)" }
          , children:
              [ DOM.img { src: "https://raw.githubusercontent.com/mantinedev/mantine/master/.demo/avatars/avatar-6.png"
                        , alt: "Avatar"
                        }
              ]
          }

      ]
    }
