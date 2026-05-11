# Companion Parity v1

`companion-parity-v1` is the installed numeric-asset contract for the protected
Walters (2024) companion replication targets.

## Directory Layout

- `discrimination/oracle/`: canonical numeric CSV/RDS assets copied or
  transformed from the Walters companion MATLAB/Stata workflow.
- `discrimination/receipts/`: optional generated figure-data receipts and
  validation summaries.
- `registry/`: protected target registry and version-specific asset manifest.
- `vam/lane-b/`: VAM companion-style evidence that is installed or staged only
  after it is explicitly marked as Lane B, not protected parity.

## Protected Scope

The v1 protected registry is limited to the 13 discrimination/FDR/frontier
targets approved in Step 1.2. VAM figure targets are excluded from protected
parity until the promotion requirements in the review-response ledger are met.

## Reading Assets

Package code should locate installed assets with:

```r
system.file("extdata", "companion-parity", "v1", ..., package = "ebrecipe")
```

Developer scripts may fall back to source-tree paths, but installed vignettes
must not rely on `tests/testthat/fixtures`.
