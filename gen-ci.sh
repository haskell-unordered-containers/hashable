#!/bin/sh

# haskell-ci doesn't know how to regenerate multiple GHA workflows
haskell-ci github cabal.project
haskell-ci github --project cabal.bench.project -o .github/workflows/haskell-ci-bench.yml --github-action-name Benchmarks
