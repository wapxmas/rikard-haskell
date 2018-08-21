#!/bin/bash
export HOME=/root
export PATH=/root/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:${PATH}
stack exec update-news-exe 2>&1
