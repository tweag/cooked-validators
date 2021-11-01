#! /bin/bash
set -uo pipefail

## Runs ormolu on all .hs files in a given project; sets the ormolu_ok
## variable to `false` in case ormolu fails. It also creates an artifact
## explaining the failure inside the tests folder.
run_ormolu() {
  local proj=$1
  echo "Running ormolu on $proj"
  ormolu --mode check $(find ./$proj -name '*.hs') 2> >(tee "tests/${proj}-ormolu.artifact")
  local ormolu_res=$?

  if [[ "$ormolu_res" -eq "0" ]]; then
    rm "tests/${proj}-ormolu.artifact"
    return 0
  else
    return 1
  fi
}

## Runs the cabal tests of a project; creates artifacts with potential failures.
run_cabal_test() {
  local proj=$1
  echo "Running 'cabal run tests' on $proj"

  ## cd into the project, then generates the cabal file and run the necessary tests.
  pushd "$proj"
  hpack

  cabal run tests | tee "../tests/${proj}-cabal-test.artifact"
  local cabal_res=$?
  popd

  if [[ "$cabal_res" -eq "0" ]]; then
    rm "tests/$proj-cabal-test.artifact"
    return 0
  else
    return 1
  fi
}

## Runs hlint on a project; creates artifacts with potential failures
run_hlint() {
  local proj=$1
  echo "Running 'hlint' on $proj"

  hlint --hint="tests/hlint.yaml" ${proj} | tee "tests/${proj}-hlint.artifact"
  local hlint_res=$?

  if [[ "$hlint_res" -eq "0" ]]; then
    rm "tests/$proj-hlint.artifact"
    return 0
  else
    return 1
  fi
}

projects=("cooked-validators" "examples")
ormolu_ok=true
cabal_ok=true
hlint_ok=true
for p in ${projects[*]}; do
  run_ormolu "$p"
  if [[ "$?" -ne "0" ]]; then
    echo "[FAILURE] 'ormolu --check' failed for $p; check the respective artifact."
    ormolu_ok=false
  fi

  run_cabal_test "$p"
  if [[ "$?" -ne "0" ]]; then
    echo "[FAILURE] 'cabal run tests' failed for $p; check the respective artifact."
    cabal_ok=false
  fi

  run_hlint "$p"
  if [[ "$?" -ne "0" ]]; then
    echo "[FAILURE] 'hlint' failed for $p; check the respective artifact."
    hlint_ok=false
  fi
done

if $cabal_ok && $ormolu_ok && $hlint_ok; then
  exit 0
else
  exit 1
fi
