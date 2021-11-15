# Automatic Report Generation

The `tasty-twaudit-metadata` library
augments `tasty` to produce a reportbased on some arbitrary metadata. In our case,
it produces LaTeX which is compatible with our [`twaudit-issue`](https://github.com/tweag/audit-docs/blob/master/tex/latex/twaudit-issue.sty) LaTeX package.

## Usage

Usage is broken up in two parts: declaring tests with metadata and running tests
while producing a report. It is worth noting that you probably need a bunch
of libraries on top of `tasty` and `tasty-twaudit-metadata` to use `tasty` to its
full potential. A reasonable recomended set is:

```
- tasty
- tasty-expected-failure
- tasty-hunit
- tasty-quickcheck
- tasty-twaudit-metadata
```

### Declaring Tests

```haskell
-- Text.Heredoc gives us the nice [str| ... |] quasi-quoter, it does require
-- the respective language extension, though.
{-# LANGUAGE QuasiQuotes #-}
import Text.Heredoc (str)

-- Bring in our library qualified just so you can identity what it provides.
import qualified Test.Tasty.Metadata as TW

import Test.Tasty.ExpectedFailure -- from tasty-expected-failure
import qualified Test.Tasty.HUnit as HU -- from tasty-hunit
import qualified Test.Tasty.QuickCheck as QC -- from tasty-quickcheck

-- The following test tree declares some tests with metadata, and some tests without
-- metadata, they can be mixed together in the same tree, no problem.
testTree =
  testGroup
    "A group with some metadata tests"
    [ TW.bug -- this pushes metadata down a tree that contains a single testcase
        TW.Critical
        [str|It is paramount that two must not be three! This is
            |a first test for the metadata association.
            |Note how the \hs{bug} constructor will add the \\issue{...} header
            |that we need for latex.
            |]
        $ expectFail $
          HU.testCase "Two must not be three" $ 2 HU.@?= 3,

      HU.testCase
        "Passing test without metadata"
        (5 HU.@?= 5),

      -- Inner testGroups work too.
      testGroup
        "Inner Group"
        [ TW.vuln' -- pushes metadata in the "Vulnerabilities" section
            "important-vuln"
            TW.High
            [str|Here's a potentially dangerous vulnerability! Beware!
                |*Drama Intensifies*
                |Here we use \hs{vuln'} to add a custom label to this vulnerability.
                |]
            $ QC.testProperty
              "Whatever squared is greater than itself"
              (\x -> (x :: Integer) * x >= x),

          expectFail $ HU.testCase "Now a failing test without metadata" undefined,

          -- Lets add another test in the 'Bug' section
          TW.bug
            TW.Medium
            [str|Finally, a last very problematic bug that we can try
                |and cross-ref \Cref{important-vuln}
                |]
            $ HU.testCase "Reasonable Starting Arithmetic Assumption" (1 HU.@?= 1)
        ]
    ]

```

### Running Tests and producing a report

In this particular example, the tests are named `test-suite`; hence, we can run
them with `cabal run test-suite`. Because `tasty` supports custom command-line options,
`tasty-twaudit-metadata` can be interacted with through:

- `--save-metareport FILE`, triggering the given `FILE` to be created (or overwritten) with
  the report produced from the test tree.
- `--metareport-only all|passed|failed`, which controls which tests with associated metadata
  should actually be reported. By default, its `all`.

For example, the following command will save all metadata reports from all successful tests
into three potential files: `passed-tests-vuln.tex`, `passed-tests-bug.tex` and
`passed-tests-underspec.tex`:

```
cabal run test-suite -- --save-metareport passed-tests.tex --metareport-only passed
```

Passing the `.tex` suffix is optional, if the suffix is not there, it will be added.
The reason for creating multiple files is to give us a little more flexiblity when
assembling the report. For example, with a template like:
```latex
\section{Vulnerabilities}
\include{src/passed-tests-vuln}

\section{Implementaton Bugs}
\include{src/passed-tests-bug}
```

we can easily still add handwritten issues to each individual section.
For this particular example, this is the contents of `passed-tests-vuln.tex`:

```latex
\issue{\High}{important-vuln}{Whatever squared is greater than itself}
Here's a potentially dangerous vulnerability! Beware!
*Drama Intensifies*
Here we use \hs{vuln'} to add a custom label to this vulnerability.
```

And this is the contents of `passed-tests-bug.tex`:

```latex
\issue{\Critical}{tasty-lbl-0}{Two must not be three}
It is paramount that two must not be three! This is
a first test for the metadata association.
Note how the \hs{bug} constructor will add the \\issue{...} header
that we need for latex.


\issue{\Medium}{tasty-lbl-4}{Reasonable Starting Arithmetic Assumption}
Finally, a last very problematic bug that we can try
and cross-ref \Cref{important-vuln}
```
