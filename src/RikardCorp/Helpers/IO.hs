module RikardCorp.Helpers.IO where

  import Control.Monad.Trans.Resource
  import Control.Monad.IO.Class
  import qualified Data.Text as T
  import qualified Data.Text.IO as TIO
  import qualified System.IO as SIO

  readFileUtf8 :: FilePath -> IO T.Text
  readFileUtf8 fp = runResourceT $ do
    (key, handle) <- allocate (SIO.openFile fp SIO.ReadMode) SIO.hClose
    content <- liftIO $ do
      SIO.hSetEncoding handle SIO.utf8
      TIO.hGetContents handle
    release key
    return content
