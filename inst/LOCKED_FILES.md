# Locked files manifest

This document enumerates the **12 frozen-core `R/*.R` files** that must remain
byte-identical to v1.0.0 throughout the v2 release cycle. The lock is enforced
operationally by `.githooks/pre-commit` (which aborts any commit touching these
paths) and verified at test-time by `tests/testthat/test-frozen-checksums.R`
(which recomputes SHA256 hashes against `inst/locked-core-checksums.txt`).

The frozen-files boundary preserves the v1.0.0 deconvolution-engine behaviour
bit-exactly so that v2 statistical results reproduce v1 results without any
new round of upstream MATLAB-reference validation.

## The 12 frozen files

| # | Path | Role |
|---|------|------|
| 1 | `R/deconv-engine.R` | Top-level dispatcher for the log-spline deconvolution pipeline |
| 2 | `R/deconv-spline.R` | B-spline basis construction and design-matrix builder |
| 3 | `R/deconv-likelihood.R` | Marginal log-likelihood evaluation |
| 4 | `R/deconv-penalty.R` | Roughness-penalty terms for the spline coefficients |
| 5 | `R/deconv-constraint.R` | Density / unit-mass constraint enforcement |
| 6 | `R/deconv-density.R` | Density evaluation on the support grid |
| 7 | `R/deconv-sandwich.R` | Sandwich / robust-variance helpers |
| 8 | `R/deconv-delta-method.R` | Delta-method propagation for derived quantities |
| 9 | `R/posterior-np.R` | Non-parametric posterior helpers (P(theta <= t \| Y_j), V_j*, etc.) |
| 10 | `R/posterior-linear.R` | Linear-EB posterior helpers (Walters Ch 2.1 algebra) |
| 11 | `R/utils-logsumexp.R` | Numerically stable `logsumexp()` |
| 12 | `R/utils-numerical.R` | Numerical-stability scaffolding (clipping, bounds, etc.) |

Total: **12 files**, byte-identical to v1.0.0.

## What "byte-identical" means

The SHA256 manifest at `inst/locked-core-checksums.txt` hashes each file's
**complete byte content**. The check is the same as `shasum -a 256 R/<file>`:
every byte (including trailing newlines and leading comments) must match the
locked hash.

Mathematically, for each frozen file `f`:

```
sha256(v1[f]) === sha256(v2[f])           (full-byte equality)
sha256(v2[f]) === <line in inst/locked-core-checksums.txt>
```

Full-byte hashing was chosen over header-stripping pre-processing because:

- It matches what `shasum -a 256 -c inst/locked-core-checksums.txt` does
  directly — no pre-processing pipeline to maintain.
- It is the exact policy implemented by
  `tests/testthat/test-frozen-checksums.R` (which uses
  `digest::digest(file = ..., algo = "sha256")` — full-byte).
- It is simpler — no header-format spec to maintain.

## How the lock is enforced

1. **Operationally** — `.githooks/pre-commit` aborts any commit whose staged
   change-set intersects the locked-files list. To enable, run once:

   ```
   git config core.hooksPath .githooks
   ```

2. **In tests** — `tests/testthat/test-frozen-checksums.R` reads
   `inst/locked-core-checksums.txt`, recomputes SHA256 over each file, and
   `expect_equal()`s against the stored hash. The test is gated with
   `skip_on_cran()` because the test exists primarily for developers and CI.

## Bypassing the lock

The pre-commit hook can be bypassed with `git commit --no-verify`. **Do not
do this** unless the engine policy has been formally amended (and that
amendment carries its own decision identifier and SHA256-update discipline).
The hook is a tripwire, not access control; bypassing it is a deliberate,
traceable act.

If you are tempted to edit a frozen file because the engine is producing
"wrong" output, stop. The engine was validated bit-exact against the
upstream MATLAB reference for v1.0.0 and is correct by construction. The
bug is almost certainly in a *consumer* of the engine (in non-frozen code)
where columns, types, or units have drifted. Find the consumer and fix it
there.
