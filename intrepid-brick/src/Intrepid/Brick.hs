{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Intrepid.Brick (
    -- * App
    IntrepidApp,
    getBrickApp,

    -- * State
    IntrepidState,
    nullState,

    -- ** View
    IntrepidView (..),

    -- * Combinators
    widget,
) where

import Brick (
    App (..),
    AttrMap,
    BrickEvent,
    CursorLocation,
    EventM,
    Widget,
 )
import qualified Brick
import qualified Graphics.Vty as Vty
import Intrepid (AppT, update, view)

data IntrepidState n e = IntrepidState
    { currentApp :: IntrepidApp n e ()
    , widgets :: [Widget n]
    , attrMap :: AttrMap
    , chooseCursor :: [CursorLocation n] -> Maybe (CursorLocation n)
    }

newtype IntrepidView n e
    = IntrepidView
        ( [Widget n]
        , AttrMap
        , [CursorLocation n] -> Maybe (CursorLocation n)
        )

type IntrepidApp n e = AppT (BrickEvent n e) (IntrepidView n e) (EventM n (IntrepidState n e))

nullState :: IntrepidState n e
nullState =
    IntrepidState
        { currentApp = pure ()
        , widgets = [Brick.emptyWidget]
        , attrMap = Brick.attrMap Vty.defAttr mempty
        , chooseCursor = const Nothing
        }

getBrickApp :: IntrepidApp n e () -> App (IntrepidState n e) e n
getBrickApp app = toBrickApp $ view app >>= onView
  where
    toBrickApp appStartEvent =
        App
            { appDraw = widgets
            , appChooseCursor = chooseCursor
            , appHandleEvent = handleEvent
            , appStartEvent
            , appAttrMap = attrMap
            }
    onView = \case
        Left (IntrepidView (widgets, attrMap, chooseCursor), nextApp) ->
            Brick.put
                IntrepidState
                    { currentApp = nextApp
                    , widgets
                    , attrMap
                    , chooseCursor
                    }
        Right{} -> Brick.halt

handleEvent :: BrickEvent n e -> EventM n (IntrepidState n e) ()
handleEvent e =
    Brick.get
        >>= flip update e . currentApp
        >>= view
        >>= onView
  where
    onView = \case
        Left (IntrepidView (widgets, attrMap, chooseCursor), nextApp) ->
            Brick.put $
                IntrepidState
                    { currentApp = nextApp
                    , widgets
                    , attrMap
                    , chooseCursor
                    }
        _ -> Brick.halt

-- | Embed a static widget in a view
widget :: Widget n -> IntrepidView n e
widget w = IntrepidView ([w], Brick.attrMap Vty.defAttr mempty, const Nothing)
