module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data Game = Game
data Settings = Settings

myBigFunction :: StateT Game (ReaderT Settings (WriterT String (Except String))) ()
myBigFunction = do
   game <- get
   set <- lift ask
   lift . lift . tell $ "log this"
   lift . lift . lift . throwE $ "error"
   return ()
