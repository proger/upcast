module Upcast.Shell.Commands (cat, cat0, cat1) where

import Upcast.Shell.Types

cat = exec "cat"
cat0 = exec "cat" []
cat1 file = exec "cat" [file]
