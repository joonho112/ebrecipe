# Companion Parity Assets

This directory contains installed numeric assets for reproducing selected
Walters (2024) companion figures from package vignettes and parity tests.

The assets are not ordinary teaching datasets. They are provenance-preserving
oracles used to verify that `ebrecipe` renders companion-style figures from the
same numerical objects used in the original companion workflow.

## Versioning

- `v1/` is the first installed companion parity asset contract.
- New contracts should be added as a new version directory instead of silently
  replacing existing files.
- `manifest.csv` is a compact version index. The version-specific manifest
  inside each contract directory is the authoritative asset ledger.

## Scope

The initial protected scope covers discrimination, FDR, and decision-frontier
targets. VAM assets are kept in a separate Lane B area until public package
paths have scalar parity receipts against the companion statistics.

PNG and HTML renderings are intentionally not installed. Vignettes and tests
should render figures from numeric assets.
