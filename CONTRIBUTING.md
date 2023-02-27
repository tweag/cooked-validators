# Contributing

Thank you for investing your time in contributing to _Cooked Validators_!

## Short links to important resources

- Do you have a question?
  Post it using GitHub's [discussions board].
  Please do not use the issue tracker for those.

- Did you find a bug?
  Please report it using GitHub's [issue tracker].
  Select “bug report”.

- Do you want to request an enhancement?
  Please report it using GitHub's [issue tracker].
  Select “feature request”.

- [documentation](https://tweag.github.io/cooked-validators/)

[discussions board]: https://github.com/tweag/cooked-validators/discussions
[issue tracker]: https://github.com/tweag/cooked-validators/issues
[documentation]: https://tweag.github.io/cooked-validators/

## Running and testing

Our Nix integration provides everything one needs to hack on _Cooked Validators_
The environment `nix develop .#ci` contains the minimal software necessary to build and run it.
The environment `nix develop .#default` contains that plus everything needed for development, including [Haskell Language Server], [Ormolu], etc.

Unit tests are defined in `/cooked-validators/tests`.
The `/examples` directory contains more high-level tests under the form of full blown examples.
Running tests consists in checking in both those directories that `cabal run tests` is happy.
For instance:

```console
$ cd cooked-validators
$ cabal run tests
...
All 74 tests passed (2.22s)
$ cd ../examples
$ cabal run tests
...
All 16 tests passed (12.32s)
```

Our continuous integration checks all of this automatically.

[haskell language server]: https://github.com/haskell/haskell-language-server

## How to submit changes

## How to report a bug

Bugs are tracked using GitHub's [issue tracker].
Explain the problem and include additional details to help maintainers reproduce the problem:

- Use a clear and descriptive title for the issue to identify the problem.

- Describe the exact steps which reproduce the problem in as many details as possible.
  Ideally, provide a minimal example file accompanied by a set of Shell commands that trigger the problem.
  Include links to files or GitHub projects, or copy/pasteable snippets, which you use in those examples.
  If you're providing snippets in the issue, use Markdown code blocks.

- Describe the behaviour you observed after following the steps and point out what exactly is the problem with that behavior.
  Explain which behaviour you expected to see instead and why.

- If the problem is related to performance or memory, include a CPU profile capture with your report.

- If the problem wasn't triggered by a specific action, describe what you were doing before the problem happened and share more information using the guidelines below.

Provide more context by answering these questions:

- Which version of `cooked-validators` are you using?
  Which version of `plutus-apps` are you using?
  Can you reproduce the problem with the latest tagged version of _Cooked Validators_?
  Can you reproduce the problem with the latest commit on `main`?

- Can you reliably reproduce the issue?
  If not, provide details about how often the problem happens and under which conditions it normally happens.

- Are you running Nix to provide the environment?
  Which version of Nix are you using?
  Can you reproduce using the `nix develop .#ci` environment?

- What are the name and version of the OS you are using?

## How to request an enhancement

Enhancement requests are tracked using GitHub's [issue tracker].

Before creating enhancement suggestions, please check whether [there is already an issue tracking it][issue tracker].
When you are creating an enhancement suggestion, please include as many details as possible.
Fill in the template, including the steps that you imagine you would take if the feature you're requesting existed.

- Use a clear and descriptive title for the issue to identify the suggestion.

- Provide a step-by-step description of the suggested enhancement in as many details as possible.

- Provide specific examples to demonstrate the steps.
  Include copy/pasteable snippets which you use in those examples, as Markdown code blocks.

- Describe the current behavior and explain which behaviour you expected to see instead and why.

- Explain why this enhancement would be useful to most _Cooked Validators_ users and is not something that can or should be implemented as a community package.

- Specify which version of _Cooked Validators_ you are using.
  Specify the name and version of the OS you are using.

## Style guide / Coding conventions

### Haskell style guide

- All Haskell code is formatted with [Ormolu].

- Use explicit import lists.

- Document all your functions using [Haddock]'s syntax.

[ormolu]: https://github.com/tweag/ormolu
[haddock]: https://haskell-haddock.readthedocs.io/en/latest/

### Nix style guide

- All Nix code is formatted with [nixfmt].

- Avoid variable bindings that are not used.

- Provide documentation explaining any bit of code that is not standard.

[nixfmt]: https://github.com/serokell/nixfmt

### CI and pre-commit hooks

Most of those coding conventions are enforced automatically by our continuous integration.
The Nix environment provides pre-commit hooks that check those coding conventions in the exact same way the CI does.
