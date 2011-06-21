
module Paragrep.Globals where

import System.IO
import System.IO.Unsafe
import Data.IORef
import Control.Monad

version = "0.0.1.1"
-- progName = "help"
-- chatter_tag = " [help] "
progName = "paragrep"
chatter_tag = " [paragrep] "

chatter lvl str = 
  do verbosity <- readIORef verbosityRef
     when (verbosity >= lvl) $
       putStrLn$ chatter_tag ++ str

-- A mutable global variable tracking verbosity level (hack).
verbosityRef :: IORef Int
verbosityRef = unsafePerformIO $ newIORef 1
