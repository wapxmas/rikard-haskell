{-# LANGUAGE CPP #-}

module RikardCorp.DBConnection where

  import           Database.Persist.MySQL

#ifdef Windows
  rikardCorpDBci :: ConnectInfo
  rikardCorpDBci = defaultConnectInfo {
    connectHost = "X",
    connectUser = "X",
    connectPassword = "X",
    connectDatabase = "X"
  }
#else
  rikardCorpDBci :: ConnectInfo
  rikardCorpDBci = defaultConnectInfo {
    connectHost = "X",
    connectUser = "X",
    connectPassword = "X",
    connectDatabase = "X"
  }
#endif
