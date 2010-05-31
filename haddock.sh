cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/packages/archive/latest/doc/html'
scp -r dist/doc/html/persistent snoyberg_yesoddocs@ssh.phx.nearlyfreespeech.net:/home/public/haddock/
