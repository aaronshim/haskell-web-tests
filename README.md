# haskell-web-tests
Evaluating frameworks and tooling for different Haskell web frameworks

## Yesod
Navigate into the directory and run
```
stack build yesod-bin cabal-install --install-ghc
stack build
stack exec -- yesod devel
```
and the app should be alive at http://localhost:3000/
