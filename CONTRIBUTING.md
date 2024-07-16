# Contributing

Thank you for considering to contribute to _Cooked Validators_. Bug reports,
feature requests and pull requests are all most welcome!

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

## Running and testing

### Environment

We recommend using [Nix] to work on _Cooked Validators_. Our Nix integration
provides everything one needs to contribute:

- The environment `nix develop .#ci` contains the minimal software necessary to
  build and run _Cooked Validators_.

- The environment `nix develop .#default` contains that plus everything needed
  for development, including [Haskell Language Server], [Ormolu], etc.

Alternatively, one can work on _Cooked Validators_ without Nix, using only GHC
9.6.5 and Cabal.

### Running tests

Unit tests are defined in `/cooked-validators/tests`. They can be run using:

```console
$ cabal test all
...
All 132 tests passed (3.18s)
```

Our continuous integration checks them automatically.

## How to submit changes

Contributions are submitted by means of [pull requests]. Please follow these steps to
have your pull request considered by the maintainers:

- Describe in details why your contribution is relevant. Does it fix an existing
  bug? Is it linked to an existing issue? Is it a new feature that is useful to
  you?
  
- Make sure your pull request is of manageable size and complexity. For
  significant changes, make sure to post an issue first or discuss it on the
  [discussions board] with maintainers.

- Follow the [style guides].

- Follow the [imports](doc/IMPORTS.md) guidelines.

- Verify that all [status checks] are passing.

- When applicable, provide tests to support your new feature.

If a status check is failing, and you believe that the failure is unrelated to
your change, please leave a comment on the pull request explaining why you
believe the failure is unrelated. A maintainer will re-run the status check for
you. If we conclude that the failure was a false positive, then we will open an
issue to track that problem with our status check suite.

While the prerequisites above must be satisfied prior to having your pull
request reviewed, the reviewer/s may ask you to complete additional design work,
tests, or other changes before your pull request can be ultimately accepted.

## How to report a bug

Bugs are tracked using GitHub's [issue tracker]. Explain the problem and
include additional details to help maintainers reproduce the issue:

- Use a clear and descriptive title for the issue.

- Describe the exact steps which reproduce the problem. Ideally, provide a
  minimal example file accompanied by a set of Shell commands that trigger the
  problem. Include links to files or GitHub projects, or copy/pasteable
  snippets, which you use in those examples. If you're providing snippets in
  the issue, use Markdown code blocks.

- Describe the behaviour you observed after following the steps and point out
  what exactly is the problem with that behavior.  Explain which behaviour you
  expected to see instead and why.

- If the problem is related to performance or memory, include a CPU profile
  capture with your report.

- If the problem wasn't triggered by a specific action, describe what you were
  doing before the problem happened and share more information using the
  guidelines below.

Provide more context by answering these questions:

- Which version of `cooked-validators` are you using? Can you reproduce the
  problem with the latest tagged version of _Cooked Validators_? With the latest
  commit on `main`?

- Can you reliably reproduce the issue? If not, provide details about how often
  the problem happens and under which conditions it normally happens.

- Are you running Nix to provide the environment? Which version of Nix are you
  using? Can you reproduce using the `nix develop .#ci` environment?

- What are the name and version of the OS you are using?

## How to request an enhancement

Enhancement requests are tracked using GitHub's [issue tracker].

Before creating enhancement suggestions, please check whether [there is already
an issue tracking it][issue tracker]. Make sure that you are using the most
recent version of the tool and that the [changelog](CHANGELOG.md) does not
already feature your request in the unreleased section.

When you are creating an enhancement request, follow these guidelines:

- Include as many details as possible. Fill in the template and phrase any
  reasons explaining why the proposed enhancement is relevant.

- Use a clear and descriptive title for the issue to identify the suggestion.

- Provide a step-by-step description of the suggested enhancement as you would
  like to see it unfold.

- Provide specific examples to demonstrate the steps. Include copy/pasteable
  snippets which you use in those examples, as Markdown code blocks.

- Describe the current behavior and explain which behaviour you expected to see
  instead and why.

- Explain why this enhancement would be useful to most _Cooked Validators_ users
  and is not something that can or should be implemented as a community package.

## Style guides / Coding conventions

[style guides]: #style-guides--coding-conventions

### Haskell style guide

- All Haskell code is formatted with [Ormolu].

- Document all your functions using [Haddock]'s syntax.

### Nix style guide

- All Nix code is formatted with [nixfmt].

- Avoid variable bindings that are not used.

- Provide documentation explaining any bit of code that is not standard.

### CI and pre-commit hooks

Most of those coding conventions are enforced automatically by our continuous
integration. The Nix environment provides pre-commit hooks that check those
coding conventions in the exact same way the CI does.

[discussions board]: https://github.com/tweag/cooked-validators/discussions
[issue tracker]: https://github.com/tweag/cooked-validators/issues
[pull requests]: https://github.com/tweag/cooked-validators/pulls

[status checks]: https://help.github.com/articles/about-status-checks/

[haskell language server]: https://github.com/haskell/haskell-language-server
[nix]: https://nixos.org/
[ormolu]: https://github.com/tweag/ormolu
[haddock]: https://haskell-haddock.readthedocs.io/en/latest/
[nixfmt]: https://github.com/serokell/nixfmt
