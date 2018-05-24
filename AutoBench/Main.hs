


import qualified Options.Applicative as OPTS
import Language.Haskell.Interpreter
import Control.Exception.Base

import AutoBench.Hint 
import AutoBench.UserInputChecks
import AutoBench.AbstractSyntax
import AutoBench.Types
import AutoBench.Utils



runAndHandle :: Interpreter a -> IO a
runAndHandle  = (either throwIO return =<<) . runInterpreter


main :: IO () 
main = do
  --args <- OPTS.customExecParser (OPTS.prefs OPTS.showHelpOnError) $ clArgsParser
  inps <- runAndHandle $ userInputCheck "./Input.hs"
  print inps