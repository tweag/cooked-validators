#! /bin/bash
set -uo pipefail

show_help() {
  cat <<EOF
usage: ./tests/run-tests.sh [--ci]

  Note the script is ran from the repo root; Running without --ci
  will run "ormolu --mode inplace" and fix offending files.

  Options:
    --ci      Instructs the script to save artifacts and
              only checks syntax, instead of fixing it.
EOF
}

ci=false
while [[ $# -ne "0" ]]; do
  case $1 in
    --ci) ci=true; shift;;
    *) show_help; exit 1;;
  esac
done

## Runs ormolu on all .hs files in a given project; sets the ormolu_ok
## variable to `false` in case ormolu fails. It also creates an artifact
## explaining the failure inside the tests folder.
run_ormolu() {
  local proj=$1
  echo "Running fourmolu on $proj"
  local ormolu_res=0
  if $ci; then
    fourmolu --mode check $(find ./$proj -name '*.hs') 2> >(tee "tests/${proj}-ormolu.artifact")
    ormolu_res=$?
  else 
    fourmolu --mode inplace $(find ./$proj -name '*.hs') 
    ormolu_res=$?
  fi 

  if $ci && [[ "$ormolu_res" -eq "0" ]]; then
    rm "tests/${proj}-ormolu.artifact"
  fi

  return $ormolu_res
}

## Runs the cabal tests of a project; creates artifacts with potential failures.
run_cabal_test() {
  local proj=$1
  echo "Running 'cabal run tests' on $proj"

  ## cd into the project, then generates the cabal file and run the necessary tests.
  pushd "$proj"

  local cabal_res=0
  if $ci; then
    cabal run tests | tee "../tests/${proj}-cabal-test.artifact"
    cabal_res=$?
  else
    cabal run tests
    cabal_res=$?
  fi
  
  popd

  if $ci && [[ "$cabal_res" -eq "0" ]]; then
    rm "tests/$proj-cabal-test.artifact"
  fi

  return $cabal_res
}

## Runs hlint on a project; creates artifacts with potential failures
run_hlint() {
  local proj=$1
  echo "Running 'hlint' on $proj"

  local hlint_res=0
  if $ci; then
    hlint --hint="tests/hlint.yaml" ${proj} | tee "tests/${proj}-hlint.artifact"
    hlint_res=$?
  else
    hlint --hint="tests/hlint.yaml" ${proj}
    hlint_res=$?
  fi

  if $ci && [[ "$hlint_res" -eq "0" ]]; then
    rm "tests/$proj-hlint.artifact"
  fi

  return $hlint_res
}

projects=("cooked-validators" "examples")
ormolu_ok=true
cabal_ok=true
hlint_ok=true

for p in ${projects[*]}; do
  hpack "$p"
done

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
