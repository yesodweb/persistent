## persistent-template

Provides Template Haskell helpers for persistent. For more information, see
[the chapter in the Yesod book](http://www.yesodweb.com/book/persistent).

### code organization

The TH.hs module contains code generators.
persistent-template uses `EntityDef`s that it gets from the quasi-quoter.
The quasi-quoter is in persistent Quasi.hs
Similarly mant of the types come from the persistent library
