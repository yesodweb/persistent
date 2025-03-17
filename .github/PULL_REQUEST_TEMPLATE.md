Before submitting your PR, check that you've:

- [ ] Documented new APIs with [Haddock markup](https://www.haskell.org/haddock/doc/html/index.html)
- [ ] Added [`@since` declarations](http://haskell-haddock.readthedocs.io/en/latest/markup.html#since) to the Haddock
- [ ] Ran `fourmolu` on any changed files (`restyled` will do this for you, so
  accept the suggested changes if it makes them)
- [ ] Adhered to the code style (see the `.editorconfig` and `fourmolu.yaml` files for details)

After submitting your PR:

- [ ] Update the Changelog.md file with a link to your PR
- [ ] Bumped the version number if there isn't an `(unreleased)` on the Changelog
- [ ] Check that CI passes (or if it fails, for reasons unrelated to your change, like CI timeouts)

<!---Thanks so much for contributing! :)

_If these checkboxes don't apply to your PR, you can delete them_-->
