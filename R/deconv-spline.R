# Build the spline basis on the fixed support grid used by the deconvolution
# optimizer. The Walters replication normalizes this basis before penalization.
.eb_spline_basis <- function(support, n_knots = 5L) {
  .eb_validate_vector_numeric(support, "support")
  .eb_validate_scalar_numeric(n_knots, "n_knots", allow_na = FALSE)

  support <- as.numeric(support)
  n_knots <- as.integer(n_knots)

  if (any(!is.finite(support))) {
    stop("`support` must be finite.", call. = FALSE)
  }

  if (length(unique(support)) < 2L) {
    stop("`support` must contain at least two distinct points.", call. = FALSE)
  }

  if (!is.finite(n_knots) || n_knots < 1L) {
    stop("`n_knots` must be a positive integer.", call. = FALSE)
  }

  if (length(support) <= n_knots) {
    stop("`support` must contain more points than `n_knots`.", call. = FALSE)
  }

  # Start from a natural spline basis on the support grid.
  Q_raw <- splines::ns(support, df = n_knots)
  # Match the Walters MATLAB basis convention: center ns() columns and
  # scale each column to unit Euclidean norm before penalization.
  Q_centered <- scale(Q_raw, center = TRUE, scale = FALSE)
  Q <- apply(Q_centered, 2L, function(w) {
    # Unit-norm columns make the ridge penalty comparable across spline
    # directions instead of depending on the raw basis scale.
    norm <- sqrt(sum(w^2))
    if (!is.finite(norm) || norm == 0) {
      stop("Encountered a zero-norm spline basis column.", call. = FALSE)
    }
    w / norm
  })

  unname(as.matrix(Q))
}
