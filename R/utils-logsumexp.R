# These helpers centralize log-domain accumulation used across the posterior and
# likelihood code. The exported scalar helper is user-facing; the row-wise
# helper exists so matrix code can stay numerically stable without reimplementing
# the same edge-case handling.
#' Compute a Stable Log-Sum-Exp
#'
#' Numerically stable scalar log-sum-exp helper.
#'
#' @param x A numeric vector on the log scale.
#'
#' @returns A numeric scalar.
#' @export
eb_log_sum_exp <- function(x) {
  .eb_validate_vector_numeric(x, "x")

  if (length(x) == 0L) {
    return(-Inf)
  }

  if (any(is.infinite(x) & x > 0)) {
    return(Inf)
  }

  if (all(is.infinite(x) & x < 0)) {
    return(-Inf)
  }

  # Subtract the row maximum before exponentiating to avoid overflow, then add
  # it back on the log scale after accumulation.
  m <- max(x)
  m + log(sum(exp(x - m)))
}

# Internal row-wise variant of the same identity. It handles degenerate rows
# explicitly so posterior code can operate on large likelihood matrices without
# sprinkling special-case checks everywhere.
.eb_row_log_sum_exp <- function(x) {
  if (!is.matrix(x)) {
    stop("x must be a matrix.", call. = FALSE)
  }

  if (nrow(x) == 0L) {
    return(numeric(0))
  }
  if (ncol(x) == 0L) {
    return(rep(-Inf, nrow(x)))
  }

  pos_inf <- rowSums(is.infinite(x) & x > 0, na.rm = TRUE) > 0
  neg_inf <- rowSums(is.infinite(x) & x < 0, na.rm = FALSE) == ncol(x)
  neg_inf[is.na(neg_inf)] <- FALSE

  out <- rep(NA_real_, nrow(x))
  out[pos_inf] <- Inf
  out[neg_inf] <- -Inf

  regular <- !(pos_inf | neg_inf)
  if (any(regular)) {
    x_regular <- x[regular, , drop = FALSE]
    # Apply the same max-shift trick row by row only where a finite answer
    # should exist.
    m <- apply(x_regular, 1, max)
    out[regular] <- m + log(rowSums(exp(x_regular - m)))
  }

  out
}
