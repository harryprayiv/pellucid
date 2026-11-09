module Main (main) where

import Prelude

import App.Backend.Tauri as Tauri
import App.Monad (runAppM)
import Component.Root as Root
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let env = { backend: Tauri.backend }
  void $ runUI (H.hoist (runAppM env) Root.component) unit body
