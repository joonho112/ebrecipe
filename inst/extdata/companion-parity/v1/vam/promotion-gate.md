# VAM Promotion Gate

This document records the conditions required before value-added model (VAM)
targets may move from Lane B companion-style teaching status into the protected
Lane A companion parity registry.

## Current Status

VAM is not a protected parity family in `companion-parity-v1`.

The current VAM vignette and plotting helpers use bundled simulation/package
data. They are allowed to generate companion-style unconditional, conditional,
and simulation-truth figures. They must not claim exact protected parity with
the restricted Boston administrative application.

## Promotion Requirements

A VAM target can be promoted only when all of the following conditions hold.

1. Versioned source artifacts are available:
   - school-level estimates;
   - standard errors or the full VCE matrix;
   - sector/covariate data;
   - source scripts;
   - exported scalar ledgers.
2. Every source artifact has a stable SHA-256 digest.
3. The scalar parity ledger has all required rows in
   `deferred-vam-scalar-targets.csv`, with all required rows passing.
4. Figure-data receipts exist for candidate VAM figures, including layer row
   counts, summary row counts, source artifact IDs, and digests.
5. Documentation separates these cases:
   - companion simulation parity;
   - restricted Boston administrative parity;
   - simulation-only truth diagnostics.
6. The protected registry receives VAM rows only after the previous conditions
   are met and the supervisor explicitly approves promotion.

## Current Blockers

- The original Boston administrative data are restricted and are not shipped
  with `ebrecipe`.
- Current VAM figures are generated from bundled package data.
- `fig_unconditional_eb`, `fig_conditional_eb`, and `vam_truth_shrinkage` are
  deferred candidate identifiers, not protected target IDs.
- The truth-shrinkage figure requires simulated latent truth and cannot become
  an observed-data Boston parity target.

## Promotion Output

When promotion is approved, create a new contract or update a future contract
version rather than silently mutating the current protected registry. The
promotion should produce:

- protected registry rows;
- source asset ledger rows;
- digest ledger rows;
- row-count ledger rows;
- target-to-asset ledger rows;
- receipt RDS files;
- strict tests requiring `source_receipt` for promoted targets.

