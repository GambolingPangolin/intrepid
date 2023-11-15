{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Brick (BrickEvent (VtyEvent), Widget, (<+>), (<=>))
import qualified Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Control.Monad (forever, void)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as Text
import Data.These (These (..))
import Graphics.Vty (Event (EvKey), Key (KChar))
import Intrepid (AppT, attach, combine, emap, match, vmap)
import Intrepid.Brick (IntrepidView)
import qualified Intrepid.Brick as IB

main :: IO ()
main =
    void $
        Brick.defaultMain @()
            (IB.getBrickApp $ adaptApp app)
            IB.nullState

adaptApp ::
    (Functor m) =>
    AppT Key (Widget n) m () ->
    AppT (BrickEvent n e) (IntrepidView n e) m ()
adaptApp = emap getKey . vmap IB.widget
  where
    getKey = \case
        VtyEvent (EvKey k _) -> Just k
        _ -> Nothing

app :: (Monad m) => AppT Key (Widget n) m ()
app = do
    void $ match (panel "Hello!") anyChar
    loop
  where
    loop =
        combine (<=>) switches attempts `attach` char 'q'
            >>= \case
                This{} -> loop
                _ -> pure ()
    switches = void $ combine (<+>) (mkApp 'a') (mkApp 'b')

attempts :: (Monad m) => AppT Key (Widget n) m ()
attempts = loop mempty
  where
    loop xs =
        attach (match (display xs) anyChar) (char 'x')
            >>= loop . \case
                This x -> take 10 (x : xs)
                _ -> mempty
    display xs =
        panel . Text.pack $
            "Last 10 attempts: " <> (L.intersperse ',' . reverse) xs

mkApp :: (Monad m) => Char -> AppT Key (Widget n) m b
mkApp c = forever $ stage stageOn c >> stage stageOff c
  where
    stageOn = Text.pack $ "on (" <> [c] <> ")"
    stageOff = Text.pack $ "off (" <> [c] <> ")"

-- | Produce when the given char is pressed
stage ::
    (Applicative m) =>
    Text ->
    Char ->
    AppT Key (Widget n) m ()
stage stageName = match (panel stageName) . char

panel :: Text -> Widget n
panel = Brick.joinBorders . border . center . Brick.txt

-- | Event filter matching any keypress resulting in a 'Char'
anyChar :: Key -> Maybe Char
anyChar = \case
    KChar k -> Just k
    _ -> Nothing

-- | Event filter matching a specific character keypress
char :: Char -> Key -> Maybe ()
char c = \case
    KChar k | k == c -> Just ()
    _ -> Nothing
