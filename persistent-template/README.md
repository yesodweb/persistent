## persistent-template

Provides Template Haskell helpers for persistent. For more information, see
[the chapter in the Yesod book](http://www.yesodweb.com/book/persistent).

### code organization

The TH.hs module contains code generators.
persistent-template uses `EntityDef`s that it gets from the quasi-quoter.
The quasi-quoter is in persistent Quasi.hs
Similarly many of the types come from the persistent library

### Development tips

To get a better idea of what code you're generating, you can output the content of Template Haskell expressions to a file:

```
stack test persistent-template --ghc-options='-ddump-splices -ddump-to-file'
```

The output will be in the `.stack-work` directory. The exact path will depend on your specific setup, but if you search for files ending in `.dump-splices` you'll find the output (`find .stack-work -type f -name '*.dump-splices'`)

If you make changes to the generated code, it is highly recommended to compare the output with your changes to output from `master` (even better if this diff is included in your PR!). Seemingly small changes can have dramatic changes on the generated code. 

For example, embedding an `EntityDef` in a function that was called for every field of that `Entity` made the number of generated lines O(N^2) for that function—very bad!
