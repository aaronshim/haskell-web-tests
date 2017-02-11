# haskell-web-tests
Evaluating frameworks and tooling for different Haskell web frameworks

## Yesod
Navigate into the directory and run
```
stack build yesod-bin cabal-install --install-ghc
stack build
stack exec -- yesod devel
```
and the app should be alive at `http://localhost:8080/` (or whatever port number the environmet variable `$PORT` is set to)

## Spock
Navigate into the directory and run
```
stack build
stack exec
```
and the app should be alive at `http://localhost:8080/`
