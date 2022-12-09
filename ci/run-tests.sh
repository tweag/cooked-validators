set -x # Debug CI
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

# if $ci; then
#   pushd ..
#   cabal update
#   popd || exit
# fi

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

## Runs the cabal build for a project
run_cabal_build() {
  local proj=$1
  echo "Running 'cabal build' on $proj"

  pushd "$proj" || exit
  local cabal_res=0
  if $ci; then
    cabal build | tee "../${proj}-cabal-build.out"
    cabal_res=$?
    echo "cabal_build:$cabal_res" >> "../${proj}-cabal-build.res"
  else
    cabal build
    cabal_res=$?
  fi
  popd || exit

  return $cabal_res
}

## Runs the cabal tests of a project; creates artifacts with potential failures.
run_cabal_test() {
  local proj=$1
  echo "Running 'cabal run tests' on $proj"

  pushd "$proj" || exit
  local cabal_res=0
  if $ci; then
    cabal run tests | tee "../${proj}-cabal-test.out"
    cabal_res=$?
    echo "run_cabal_test:$cabal_res" >> "../${proj}-cabal-test.res"
  else
    cabal run tests
    cabal_res=$?
  fi
  popd || exit

  return $cabal_res
}

projects=("cooked-validators" "examples" "pirouette-plutusir")
overall_ok=true

for p in "${projects[@]}"; do
  cabal_build_ok=true
  cabal_test_ok=true

  if ! run_cabal_build "$p"; then
    echo "[FAILURE] 'cabal build' failed for $p; check the respective artifact."
    cabal_build_ok=false
  fi

  if $cabal_build_ok; then
    if ! run_cabal_test "$p"; then
      echo "[FAILURE] 'cabal run tests' failed for $p; check the respective artifact."
      cabal_test_ok=false
    fi
  fi

  if $ci; then
    if ! $cabal_build_ok; then
      overall_ok=false
    fi
  else
    if ! ($cabal_build_ok && $cabal_test_ok); then
      overall_ok=false
    fi
  fi
done

if $overall_ok; then
  exit 0
else
  exit 1
fi
