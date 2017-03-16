module Main where

import Data.ByteString.Char8 (pack)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig { jsPort = Just 8023
                              , jsStatic = Just "assets"
                              , jsAddr = Just $ pack "0.0.0.0"
                              } setup

setup :: Window -> UI ()
setup window = do
    set UI.title "Hello World!" $ return window
    button <- set UI.text "Click me!" UI.button
    getHead window #+ [set (UI.attr "class") "pageStyle" . set UI.text ".table-cell {padding: 1px;}" $ mkElement "style"]
    getBody window #+ [element button, grid $ map (map (flip (set UI.text) UI.p . show)) [[1..10], [101..110]]]
    on UI.click button . const . set UI.text "I have been clicked!" $ element button
