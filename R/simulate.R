#' Simulate value-added data with known ground truth
#'
#' Generates a synthetic value-added (VAM) panel with known unit effects
#' \eqn{\theta_j} and student-level outcomes \eqn{y_{ij}}, returning a
#' three-table `eb_sim` object (`students`, `schools`, `dgp`). Designed for
#' tutorials, regression tests, and end-to-end EB workflow verification where
#' having access to the truth is essential.
#'
#' @param n_units Positive integer; number of units (schools) \eqn{J} to
#'   simulate. Default `50`.
#' @param n_obs Positive integer; total number of observations (students)
#'   \eqn{N} to simulate. Default `2500`.
#' @param sigma_theta Non-negative numeric scalar; standard deviation of the
#'   latent unit signal \eqn{\theta_j \sim N(0, \sigma_\theta^2)}. Default
#'   `0.20`.
#' @param design Character scalar; `"balanced"` allocates roughly equal
#'   numbers of students per unit. `"unbalanced"` uses a discrete-choice
#'   assignment rule driven by school-specific utility shocks.
#' @param groups Optional named list. The current public schema supports a
#'   `charter` sub-list with numeric `share` (in \eqn{[0, 1]}) and numeric
#'   `boost` (additive mean shift to \eqn{\theta_j} for charter units).
#' @param seed Optional integer random seed. When supplied, the function
#'   saves and restores `.GlobalEnv$.Random.seed` on exit so the caller's
#'   RNG stream is not disturbed.
#' @param J Optional alias for `n_units`, kept for workshop-style notation
#'   (Walters writes \eqn{J} for the number of units). Overrides `n_units`
#'   when supplied.
#' @param N Optional alias for `n_obs`, kept for workshop-style notation
#'   (Walters writes \eqn{N} for the number of observations). Overrides
#'   `n_obs` when supplied.
#'
#' @returns An `eb_sim` object (S3 list) with three components:
#' \describe{
#'   \item{`students`}{Data frame -- observation-level records (`student_id`, `school_id`, `x` covariate, `theta_true`, `y` outcome, `charter`, `group`); always \eqn{N} rows.}
#'   \item{`schools`}{Data frame -- unit-level latent truth and assignment components (`school_id`, `theta`, `theta_true`, `delta`, `gamma`, `charter`, `group`, `n_students`); always \eqn{J} rows.}
#'   \item{`dgp`}{Named list -- compact record of the simulation settings used to generate the draw (`n_units`, `n_obs`, `sigma_theta`, `design`, `groups`, `seed`, plus charter-related counts). Sufficient to reproduce the draw exactly.}
#' }
#'
#' @details
#' The data-generating process draws unit effects
#' \deqn{\theta_j \sim N(0, \sigma_\theta^2),}
#' a student-level covariate \eqn{x_i \sim N(0,1)}, and outcomes
#' \deqn{y_{ij} = \theta_{j(i)} + x_i + \varepsilon_i, \quad \varepsilon_i \sim N(0,1).}
#' This matches the canonical homoskedastic VAM setup of Walters Ch 2.2
#' (eq. 5-6).
#'
#' In the unbalanced design, school assignment is generated from
#' school-specific utility components \eqn{\delta_j}, \eqn{\gamma_j}, and
#' Gumbel shocks, which produces realistic unequal school sizes. The optional
#' `groups$charter` block adds an additive mean shift `boost` to
#' \eqn{\theta_j} for a `share` fraction of units, enabling group-conditional
#' shrinkage demos (Walters Ch 6.2).
#'
#' Although `eb_simulate()` returns an `eb_sim` object rather than an
#' `eb_estimates` object directly, its `students` table is the canonical input
#' to [eb_estimate_fe()] for end-to-end shrinkage workflows; for that reason
#' it is grouped with the other estimate-layer constructors in the
#' `eb_estimates` family.
#'
#' @family eb_estimates
#' @seealso [eb_input()], [eb_estimate_fe()], [eb_estimate_groups()],
#'   [eb_vam()], [eb()], [vam_simulated], [vam_schools]
#'
#' @examples
#' # Reproducible 8-school, 80-student draw for a quick demo.
#' sim <- eb_simulate(n_units = 8, n_obs = 80, seed = 1)
#' nrow(sim$students)
#' head(sim$schools$theta_true)
#' sim$dgp$design
#'
#' # Round-trip: simulate -> estimate FE.
#' est <- eb_estimate_fe(y ~ x | school_id, data = sim$students)
#' head(est$theta_hat)
#'
#' @export
eb_simulate <- function(n_units = 50, n_obs = 2500,
                        sigma_theta = 0.20,
                        design = c("balanced", "unbalanced"),
                        groups = NULL,
                        seed = NULL,
                        J = NULL,
                        N = NULL) {
  if (!is.null(J)) {
    n_units <- J
  }

  if (!is.null(N)) {
    n_obs <- N
  }

  n_units <- .eb_validate_sim_count(n_units, "n_units")
  n_obs <- .eb_validate_sim_count(n_obs, "n_obs")

  .eb_validate_scalar_numeric(sigma_theta, "sigma_theta", allow_na = FALSE)
  if (!is.finite(sigma_theta) || sigma_theta < 0) {
    stop("`sigma_theta` must be finite and non-negative.", call. = FALSE)
  }

  design <- if (identical(design, c("balanced", "unbalanced"))) {
    "unbalanced"
  } else {
    match.arg(design)
  }

  group_spec <- .eb_simulation_groups(groups)
  charter_share <- group_spec$charter$share
  charter_boost <- group_spec$charter$boost
  n_charter <- as.integer(round(n_units * charter_share))

  if (!is.null(seed)) {
    seed <- .eb_validate_sim_count(seed, "seed")
  }

  simulate_once <- function() {
    school_id <- seq_len(n_units)
    theta_base <- stats::rnorm(n_units, mean = 0, sd = sigma_theta)
    delta <- stats::rnorm(n_units, mean = 0, sd = 1)
    gamma <- stats::rnorm(n_units, mean = 0, sd = 0.5)
    charter_ids <- if (n_charter > 0L) {
      sort(sample.int(n_units, size = n_charter, replace = FALSE))
    } else {
      integer(0)
    }
    charter <- school_id %in% charter_ids
    theta_true <- theta_base + ifelse(charter, charter_boost, 0)

    student_id <- seq_len(n_obs)
    x <- stats::rnorm(n_obs, mean = 0, sd = 1)

    if (design == "balanced") {
      school_assignment <- sample(rep(school_id, length.out = n_obs), size = n_obs)
    } else {
      utility_shocks <- -log(-log(matrix(stats::runif(n_obs * n_units), nrow = n_obs)))
      utilities <- sweep(matrix(x, nrow = n_obs, ncol = n_units), 2L, gamma, `*`)
      utilities <- sweep(utilities, 2L, delta, `+`)
      utilities <- utilities + utility_shocks
      school_assignment <- max.col(utilities, ties.method = "first")
    }

    theta_student <- theta_true[school_assignment]
    y <- theta_student + x + stats::rnorm(n_obs, mean = 0, sd = 1)
    group <- ifelse(charter, "charter", "public")
    n_students <- tabulate(school_assignment, nbins = n_units)

    students <- data.frame(
      student_id = student_id,
      school_id = school_assignment,
      x = x,
      theta_true = theta_student,
      y = y,
      charter = charter[school_assignment],
      group = group[school_assignment],
      stringsAsFactors = FALSE
    )

    schools <- data.frame(
      school_id = school_id,
      theta = theta_true,
      theta_true = theta_true,
      delta = delta,
      gamma = gamma,
      charter = charter,
      group = group,
      n_students = as.integer(n_students),
      stringsAsFactors = FALSE
    )

    dgp <- list(
      J = n_units,
      N = n_obs,
      n_units = n_units,
      n_obs = n_obs,
      mu_theta = 0,
      sigma_theta = sigma_theta,
      sigma_delta = 1,
      sigma_gamma = 0.5,
      sigma_x = 1,
      beta = 1,
      sigma_y = 1,
      design = design,
      groups = group_spec,
      charter_share = charter_share,
      n_charter = n_charter,
      charter_count = n_charter,
      charter_boost = charter_boost,
      charter_ids = charter_ids,
      seed = seed
    )

    new_eb_sim(students = students, schools = schools, dgp = dgp)
  }

  .eb_with_seed(seed, simulate_once)
}

.eb_validate_sim_count <- function(x, name) {
  .eb_validate_scalar_numeric(x, name, allow_na = FALSE)

  if (!is.finite(x) || x < 1 || x != as.integer(x)) {
    stop(sprintf("`%s` must be a positive integer scalar.", name), call. = FALSE)
  }

  as.integer(x)
}

.eb_simulation_groups <- function(groups) {
  default_groups <- list(
    charter = list(
      share = 0.14,
      boost = 0.15
    )
  )

  if (is.null(groups)) {
    return(default_groups)
  }

  if (!is.list(groups)) {
    stop("`groups` must be NULL or a named list.", call. = FALSE)
  }

  charter <- groups$charter %||% list()
  if (!is.list(charter)) {
    stop("`groups$charter` must be a list.", call. = FALSE)
  }

  share <- charter$share %||% default_groups$charter$share
  boost <- charter$boost %||% default_groups$charter$boost

  .eb_validate_scalar_numeric(share, "groups$charter$share", allow_na = FALSE)
  .eb_validate_scalar_numeric(boost, "groups$charter$boost", allow_na = FALSE)

  if (!is.finite(share) || share < 0 || share > 1) {
    stop("`groups$charter$share` must lie in [0, 1].", call. = FALSE)
  }

  if (!is.finite(boost)) {
    stop("`groups$charter$boost` must be finite.", call. = FALSE)
  }

  list(
    charter = list(
      share = as.numeric(share),
      boost = as.numeric(boost)
    )
  )
}

.eb_with_seed <- function(seed, fn) {
  if (is.null(seed)) {
    return(fn())
  }

  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit(
    {
      if (has_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  set.seed(seed)
  fn()
}
