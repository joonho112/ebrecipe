# Compare q-value and posterior-mean selection at the same selection share,
# rather than at a common numerical cutoff.
.eb_decision_frontier <- function(q_values, posterior_mean, selection_share = 0.20) {
  .eb_validate_vector_numeric(q_values, "q_values")
  .eb_validate_vector_numeric(posterior_mean, "posterior_mean")
  .eb_control_probability(
    selection_share,
    "selection_share",
    lower = 0,
    upper = 1,
    include_lower = TRUE,
    include_upper = TRUE
  )

  q_values <- as.numeric(q_values)
  posterior_mean <- as.numeric(posterior_mean)
  .eb_validate_matching_length(
    q_values,
    posterior_mean,
    "q_values",
    "posterior_mean"
  )

  n_units <- length(q_values)
  if (n_units == 0L) {
    stop("`q_values` and `posterior_mean` must have positive length.", call. = FALSE)
  }

  # Turn the requested share into one common integer selection budget for both
  # decision rules.
  n_select <- floor(selection_share * n_units)
  # Rank q-values upward and posterior means downward so each rule selects its
  # own "best" units under the same budget.
  q_selected <- .eb_select_by_rank(q_values, n_select = n_select, decreasing = FALSE)
  pm_selected <- .eb_select_by_rank(posterior_mean, n_select = n_select, decreasing = TRUE)

  if (n_select == 0L) {
    # When the share rounds down to zero, report the empty-selection frontier
    # rather than forcing a positive cutoff.
    return(data.frame(
      share = as.numeric(selection_share),
      q_cutoff = NA_real_,
      pm_cutoff = NA_real_,
      overlap = 0,
      mean_theta_star_qval = NA_real_,
      mean_theta_star_pm = NA_real_,
      max_q_pm = NA_real_
    ))
  }

  # Use tiny offsets so the reported cutoffs summarize the selected sets
  # without becoming ambiguous at exact equality boundaries.
  eps <- 1e-13

  data.frame(
    share = as.numeric(selection_share),
    # These are the marginal thresholds that reproduce the matched-share sets
    # under the deterministic tie-breaking rule below.
    q_cutoff = max(q_values[q_selected]) + eps,
    pm_cutoff = min(posterior_mean[pm_selected]) - eps,
    # Overlap is the direct agreement count between the two rules at the same
    # selection share.
    overlap = sum(q_selected & pm_selected),
    # Evaluate both selected sets on posterior means so the comparison happens
    # on one common EB utility scale.
    mean_theta_star_qval = mean(posterior_mean[q_selected]),
    mean_theta_star_pm = mean(posterior_mean[pm_selected]),
    # Record how deep the posterior-mean rule reaches into the q-value ranking.
    max_q_pm = max(q_values[pm_selected])
  )
}

.eb_select_by_rank <- function(x, n_select, decreasing = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) {
    return(logical())
  }

  n_select <- max(min(as.integer(n_select), length(x)), 0L)
  selected <- rep(FALSE, length(x))
  if (n_select == 0L) {
    return(selected)
  }

  # Break ties by original index so matched-share comparisons stay
  # deterministic and reproducible.
  ord <- if (isTRUE(decreasing)) {
    order(-x, seq_along(x))
  } else {
    order(x, seq_along(x))
  }

  selected[ord[seq_len(n_select)]] <- TRUE
  selected
}
