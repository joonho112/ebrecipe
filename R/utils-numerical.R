# These utilities provide small numerical building blocks for internal EB
# computations. They favor predictable, defensive behavior over maximum speed
# because they are mainly used in diagnostics, sandwich calculations, and other
# helper paths where stability matters more than micro-optimization.
.eb_fd_step <- function(x, step = NULL) {
  if (is.null(step)) {
    # Scale the perturbation to the magnitude of each coordinate, but keep a
    # floor so near-zero parameters still receive a usable finite-difference step.
    return(pmax(1e-4, abs(x) * 1e-4))
  }

  if (length(step) == 1L) {
    step <- rep(as.numeric(step), length(x))
  } else {
    if (length(step) != length(x)) {
      stop("step must be NULL, scalar, or have the same length as x.", call. = FALSE)
    }

    step <- as.numeric(step)
  }

  if (any(!is.finite(step)) || any(step <= 0)) {
    stop("`step` must be finite and strictly positive.", call. = FALSE)
  }

  step
}

# Central-difference Hessian approximation for small internal optimization
# problems. Symmetry is enforced by filling the upper triangle and mirroring it.
.eb_numerical_hessian <- function(fn, x, ..., step = NULL) {
  .eb_validate_vector_numeric(x, "x")
  x <- as.numeric(x)
  h <- .eb_fd_step(x, step)
  f0 <- fn(x, ...)

  if (!is.numeric(f0) || length(f0) != 1L || !is.finite(f0)) {
    stop("fn must return a finite numeric scalar.", call. = FALSE)
  }

  n <- length(x)
  out <- matrix(0, nrow = n, ncol = n)

  for (i in seq_len(n)) {
    xp <- xm <- x
    xp[i] <- xp[i] + h[i]
    xm[i] <- xm[i] - h[i]
    fp <- fn(xp, ...)
    fm <- fn(xm, ...)

    if (!is.numeric(fp) || length(fp) != 1L || !is.finite(fp) ||
        !is.numeric(fm) || length(fm) != 1L || !is.finite(fm)) {
      stop("All perturbed fn evaluations must return finite numeric scalars.", call. = FALSE)
    }

    out[i, i] <- (fp - 2 * f0 + fm) / (h[i]^2)

    if (i < n) {
      for (j in seq.int(i + 1L, n)) {
        xpp <- x
        xpm <- x
        xmp <- x
        xmm <- x
        xpp[c(i, j)] <- xpp[c(i, j)] + c(h[i], h[j])
        xpm[i] <- xpm[i] + h[i]
        xpm[j] <- xpm[j] - h[j]
        xmp[i] <- xmp[i] - h[i]
        xmp[j] <- xmp[j] + h[j]
        xmm[c(i, j)] <- xmm[c(i, j)] - c(h[i], h[j])

        fpp <- fn(xpp, ...)
        fpm <- fn(xpm, ...)
        fmp <- fn(xmp, ...)
        fmm <- fn(xmm, ...)

        if (!is.numeric(fpp) || length(fpp) != 1L || !is.finite(fpp) ||
            !is.numeric(fpm) || length(fpm) != 1L || !is.finite(fpm) ||
            !is.numeric(fmp) || length(fmp) != 1L || !is.finite(fmp) ||
            !is.numeric(fmm) || length(fmm) != 1L || !is.finite(fmm)) {
          stop("All perturbed fn evaluations must return finite numeric scalars.", call. = FALSE)
        }

        # The mixed partial uses the usual four-corner central-difference
        # stencil, which is accurate enough for the small matrices where this
        # helper is used.
        mixed <- (
          fpp -
            fpm -
            fmp +
            fmm
        ) / (4 * h[i] * h[j])

        out[i, j] <- mixed
        out[j, i] <- mixed
      }
    }
  }

  out
}

# Central-difference Jacobian for vector-valued internal functions. This keeps
# derivative logic consistent with the Hessian helper above.
.eb_numerical_jacobian <- function(fn, x, ..., step = NULL) {
  .eb_validate_vector_numeric(x, "x")
  x <- as.numeric(x)
  h <- .eb_fd_step(x, step)
  f0 <- fn(x, ...)

  if (!is.numeric(f0)) {
    stop("fn must return a numeric vector.", call. = FALSE)
  }

  m <- length(f0)
  n <- length(x)
  out <- matrix(0, nrow = m, ncol = n)

  for (i in seq_len(n)) {
    xp <- xm <- x
    xp[i] <- xp[i] + h[i]
    xm[i] <- xm[i] - h[i]
    fp <- fn(xp, ...)
    fm <- fn(xm, ...)

    if (!is.numeric(fp) || !is.numeric(fm) || length(fp) != m || length(fm) != m ||
        any(!is.finite(fp)) || any(!is.finite(fm))) {
      stop("fn must return a finite numeric vector of constant length.", call. = FALSE)
    }

    out[, i] <- (fp - fm) / (2 * h[i])
  }

  out
}

# Warn early when a J x M allocation may be large enough to dominate memory.
# The function returns the cell count invisibly so callers can reuse the same
# quantity for diagnostics if they want to.
.eb_warn_large_matrix <- function(n_rows, n_cols, threshold = 1e7, label = "matrix") {
  cells <- as.double(n_rows) * as.double(n_cols)

  if (!is.finite(threshold) || threshold <= 0) {
    stop("`threshold` must be finite and strictly positive.", call. = FALSE)
  }

  if (is.finite(cells) && cells > threshold) {
    warning(
      sprintf(
        "Constructing the %s with J x M = %.0f cells may require substantial memory.",
        label,
        cells
      ),
      call. = FALSE
    )
  }

  invisible(cells)
}

# Normalize vectors or matrices defensively. When a margin sum is non-finite or
# non-positive, fall back to a uniform distribution rather than propagating NaN
# rows into later posterior calculations.
.eb_safe_normalize <- function(x, margin = 1L) {
  .eb_validate_vector_numeric(as.numeric(x), "x")

  if (is.null(dim(x))) {
    total <- sum(x)
    if (!is.finite(total) || total <= 0) {
      return(rep(1 / length(x), length(x)))
    }
    return(x / total)
  }

  if (!is.matrix(x)) {
    stop("x must be a numeric vector or matrix.", call. = FALSE)
  }

  if (!margin %in% c(1L, 2L)) {
    stop("margin must be 1L or 2L.", call. = FALSE)
  }

  out <- x

  if (margin == 1L) {
    for (i in seq_len(nrow(x))) {
      total <- sum(x[i, ])
      out[i, ] <- if (!is.finite(total) || total <= 0) {
        rep(1 / ncol(x), ncol(x))
      } else {
        x[i, ] / total
      }
    }
  } else {
    for (j in seq_len(ncol(x))) {
      total <- sum(x[, j])
      out[, j] <- if (!is.finite(total) || total <= 0) {
        rep(1 / nrow(x), nrow(x))
      } else {
        x[, j] / total
      }
    }
  }

  out
}

# Clamp away from zero before logging so likelihood-style calculations can stay
# on the log scale without generating `-Inf` from tiny rounding artifacts.
.eb_safe_log <- function(x, eps = .Machine$double.xmin) {
  .eb_validate_vector_numeric(x, "x")

  if (any(is.na(x))) {
    stop("x must not contain missing values.", call. = FALSE)
  }

  log(pmax(x, eps))
}
