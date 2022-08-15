#! /usr/bin/env bash
set -uo pipefail

show_help() {
  cat <<EOF
usage: ./ci/run-tests.sh [--ci]
  Note the script is ran from the repo root; Running without --ci
  will run "ormolu --mode inplace" and fix offending files.
  Options:
    --ci      Informs the script it is running in CI; this means
              we will save the test results as a file (named <project>-cabal-test.{res,out})
              The script WILL SUCCEED IFF THE BUILD SUCCEEDED; 
              This behavior ensures that the build gets cached. 
              Another job should check the artifact/cache to decide whether
              the workflow passes or fails as a whole
EOF
}

ci=false
while [[ $# -ne "0" ]]; do
  case $1 in
    --ci) ci=true; shift;;
    *) show_help; exit 1;;
  esac
done

if $ci; then
  pushd ..
  cabal update
  popd
fi

## Each step below runs a certain program and, when running in CI, creates
## two files: ${proj}-${step}.res and ${proj}-${step}.out
## The former contains a string encoding the return code of the tool whereas
## the later contains a human-readable description of what happened
##
## The point of this is that when running in CI, we will execute all these steps
## but we "succeed" iff the build is ok. Another job will then check the resulting
## files for their exit codes. This is ugly but it ensures that the cabal build
## gets cached even if tests or ormolu fails, which means we save a lot of time 
## (and CI runner money) in the long run.

run_hpack() {
  local proj=$1
  echo "Running hpack on $proj"
  local hpack_res=0

  pushd "$proj"
  hpack | grep -v "generated"
  hpack_res=$?
  popd
  
  if $ci; then
    if [[ "$hpack_res" -ne "0" ]]; then
      echo "hpack regenerated the cabal file; please push it!" >> "./${proj}-hpack.out"
    else
      echo "hpack didn't need to regenarate; all good" >> "./${proj}-hpack.out"
    fi
    echo "run_hpack:$hpack_res" >> "./${proj}-hpack.res"
  fi 

  return $hpack_res
}

## Runs ormolu on all .hs files in a given project; sets the ormolu_ok
## variable to `false` in case ormolu fails. It also creates an artifact
## explaining the failure inside the tests folder.
run_ormolu() {
  local proj=$1
  echo "Running ormolu on $proj"
  local ormolu_res=0
  if $ci; then
    ormolu --mode check $(find ./$proj -name '*.hs') 2> >(tee "./${proj}-ormolu.out")
    ormolu_res=$?
    echo "run_ormolu:$ormolu_res" >> "./${proj}-ormolu.res"
  else 
    ormolu --mode inplace $(find ./$proj -name '*.hs') 
    ormolu_res=$?
  fi 

  return $ormolu_res
}

## Runs the cabal build for a project
run_cabal_build() {
  local proj=$1
  echo "Running 'cabal build' on $proj"

  pushd "$proj"
  local cabal_res=0
  if $ci; then
    cabal build | tee "../${proj}-cabal-build.out"
    cabal_res=$?
    echo "cabal_build:$cabal_res" >> "../${proj}-cabal-build.res"
  else
    cabal build
    cabal_res=$?
  fi
  popd

  return $cabal_res
}

## Runs the cabal tests of a project; creates artifacts with potential failures.
run_cabal_test() {
  local proj=$1
  echo "Running 'cabal run tests' on $proj"

  pushd "$proj"
  local cabal_res=0
  if $ci; then
    cabal run tests | tee "../${proj}-cabal-test.out"
    cabal_res=$?
    echo "run_cabal_test:$cabal_res" >> "../${proj}-cabal-test.res"
  else
    cabal run tests
    cabal_res=$?
  fi
  popd

  return $cabal_res
}

projects=("cooked-validators" "examples" "pirouette-plutusir")
overall_ok=true

for p in ${projects[*]}; do
  hpack_ok=true
  ormolu_ok=true
  cabal_build_ok=true
  cabal_test_ok=true

  run_hpack "$p"
  if [[ "$?" -ne "0" ]]; then
    echo "[FAILURE] 'hpack' failed for $p; check the respective artifact."
    hack_ok=false
  fi

  run_cabal_build "$p"
  if [[ "$?" -ne "0" ]]; then
    echo "[FAILURE] 'cabal build' failed for $p; check the respective artifact."
    cabal_build_ok=false
  fi

  if $cabal_build_ok; then
    run_cabal_test "$p"
    if [[ "$?" -ne "0" ]]; then
      echo "[FAILURE] 'cabal run tests' failed for $p; check the respective artifact."
      cabal_test_ok=false
    fi
  fi

  run_ormolu "$p"
  if [[ "$?" -ne "0" ]]; then
    echo "[FAILURE] 'ormolu' failed for $p; check the respective artifact."
    ormolu_ok=false
  fi

  if $ci; then
    if ! $cabal_build_ok; then
      overall_ok=false
    fi
  else
    if ! ($hpack_ok && $cabal_build_ok && $cabal_test_ok && $ormolu_ok && $hpack_ok); then
      overall_ok=false
    fi
  fi    
done

if $overall_ok; then
  exit 0
else
  exit 1
fi
