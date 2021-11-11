# tasty-twaudit-metadata

Enables tasty to produce a reportbased on test metadata. Check
`tests/Spec.hs` for an example.

You can instruct tasty to create a file for you with all the collected
metadata with:

```
cabal run test-suite -- --save-report file.tex
```
