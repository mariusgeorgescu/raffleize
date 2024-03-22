[1mdiff --git a/.direnv/bin/nix-direnv-reload b/.direnv/bin/nix-direnv-reload[m
[1mindex 186ac96..f6e02de 100755[m
[1m--- a/.direnv/bin/nix-direnv-reload[m
[1m+++ b/.direnv/bin/nix-direnv-reload[m
[36m@@ -1,8 +1,19 @@[m
 #!/usr/bin/env bash[m
[31m-dir="$(realpath $(dirname ${BASH_SOURCE[0]})/../..)"[m
[31m-_nix_direnv_force_reload=1 direnv exec "$dir" true[m
[31m-direnv reload[m
[31m-# direnv reload updates the mtime of .envrc. Also update the timestamp of the[m
[31m-# profile_rc file to keep track that we actually are up to date.[m
[31m-touch $dir/.direnv/{nix,flake}-profile-*.rc[m
[32m+[m[32mset -e[m
[32m+[m[32mif [[ ! -d "/home/mariusgeorgescu/CardanoProjects/raffleize" ]]; then[m
[32m+[m[32m  echo "Cannot find source directory; Did you move it?"[m
[32m+[m[32m  echo "(Looking for "/home/mariusgeorgescu/CardanoProjects/raffleize")"[m
[32m+[m[32m  echo 'Cannot force reload with this script - use "direnv reload" manually and then try again'[m
[32m+[m[32m  exit 1[m
[32m+[m[32mfi[m
 [m
[32m+[m[32m# rebuild the cache forcefully[m
[32m+[m[32m_nix_direnv_force_reload=1 direnv exec "/home/mariusgeorgescu/CardanoProjects/raffleize" true[m
[32m+[m
[32m+[m[32m# Update the mtime for .envrc.[m
[32m+[m[32m# This will cause direnv to reload again - but without re-building.[m
[32m+[m[32mtouch "/home/mariusgeorgescu/CardanoProjects/raffleize/.envrc"[m
[32m+[m
[32m+[m[32m# Also update the timestamp of whatever profile_rc we have.[m
[32m+[m[32m# This makes sure that we know we are up to date.[m
[32m+[m[32mtouch -r "/home/mariusgeorgescu/CardanoProjects/raffleize/.envrc" "/home/mariusgeorgescu/CardanoProjects/raffleize/.direnv"/*.rc[m
[1mdiff --git a/src/TUI/UI.hs b/src/TUI/UI.hs[m
[1mindex e9b470f..c8f5221 100644[m
[1m--- a/src/TUI/UI.hs[m
[1m+++ b/src/TUI/UI.hs[m
[36m@@ -10,9 +10,12 @@[m [mimport Graphics.Vty.Input.Events[m
 import GeniusYield.GYConfig (GYCoreConfig)[m
 import GeniusYield.Types (GYPaymentSigningKey)[m
 [m
[31m-import Brick.Widgets.Core (str, vBox)[m
[32m+[m[32mimport Brick.Widgets.Core (joinBorders, str, vBox, withAttr, withBorderStyle)[m
[32m+[m
[32m+[m[32mimport Brick.Util[m
[32m+[m[32mimport Brick.Widgets.Border.Style (borderStyleFromChar)[m
[32m+[m[32mimport Graphics.Vty (blue, defAttr, green, magenta, white, yellow)[m
 [m
[31m-import Graphics.Vty (defAttr)[m
 import RaffleizeDApp.Constants (raffleizeLogoPath)[m
 import RaffleizeDApp.TxBuilding.Transactions[m
 import System.IO.Extra (readFile)[m
[36m@@ -51,8 +54,8 @@[m [mapp =[m
 tui :: IO ()[m
 tui = do[m
   initialState <- buildInitialState[m
[31m-  endState <- defaultMain app initialState[m
[31m-  print endState[m
[32m+[m[32m  _endState <- defaultMain app initialState[m
[32m+[m[32m  return ()[m
 [m
 -- Building the initial state[m
 [m
[36m@@ -73,4 +76,18 @@[m [mhandleEvent s e = case e of[m
 -- Drawing[m
 [m
 drawUI :: RaffleizeUI -> [Widget Name][m
[31m-drawUI s = [vBox [str (logo s), str "TODO"]][m
[32m+[m[32mdrawUI s =[m
[32m+[m[32m  [ vBox[m
[32m+[m[32m      [ withAttr "highlight" $ joinBorders $ withBorderStyle (borderStyleFromChar '-') $ str (logo s)[m
[32m+[m[32m      , withAttr "good" $ str "[q] - Quit"[m
[32m+[m[32m      ][m
[32m+[m[32m  ][m
[32m+[m
[32m+[m[32mappAttrMap :: AttrMap[m
[32m+[m[32mappAttrMap =[m
[32m+[m[32m  attrMap[m
[32m+[m[32m    (white `on` blue)[m
[32m+[m[32m    [ ("highlight", fg yellow)[m
[32m+[m[32m    , ("warning", bg magenta)[m
[32m+[m[32m    , ("good", white `on` green)[m
[32m+[m[32m    ][m
\ No newline at end of file[m
