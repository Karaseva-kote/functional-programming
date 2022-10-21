module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Set
import HW3.Action
import HW3.Evaluator            (eval)
import HW3.Parser               (parse)
import HW3.Pretty               (prettyValue)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "hi> "
  case minput of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just input  -> do
      exP <- getExternalPrint
--      let eitherIn = parse input
      case parse input of
        Left e  -> outputStrLn $ show e
        Right e -> do
          r <- liftIO $ runHIO (eval e) (fromList [AllowRead, AllowWrite{-, AllowTime-}])
          liftIO $ either (exP . show) (exP . show . prettyValue) r
      loop