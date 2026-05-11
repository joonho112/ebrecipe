testthat::test_that("variance-matching penalty search stays inside the requested grid", {
  fixture <- .step31_discrimination_fixture("white")
  support <- fixture$support
  Q <- ebrecipe:::.eb_spline_basis(support, n_knots = 5L)
  penalty_grid <- seq(0.001, 0.01, by = 0.001)

  selection <- ebrecipe:::.eb_select_penalty(
    target_var = ebrecipe:::.eb_bias_corrected_variance(fixture$r, fixture$s_r^2),
    theta_hat = fixture$r,
    s = fixture$s_r,
    Q = Q,
    support = support,
    target_mean = fixture$target_mean,
    penalty_grid = penalty_grid,
    mode = "engineering"
  )

  testthat::expect_true(selection$penalty > 0)
  testthat::expect_true(selection$penalty %in% penalty_grid)
  testthat::expect_equal(nrow(selection$all_results), length(penalty_grid))
  testthat::expect_lte(
    abs(sqrt(selection$fitted_var) - sqrt(selection$target_var)),
    0.10 * sqrt(selection$target_var) + 1e-8
  )
})

testthat::test_that("penalty search separates engineering warm starts from replication fresh starts", {
  support <- c(-1, 0, 1)
  Q <- matrix(
    c(
      1, 0, 0,
      1, 1, 0,
      1, 0, 1
    ),
    nrow = 3,
    byrow = TRUE
  )
  log_P <- matrix(0, nrow = 1, ncol = length(support))
  penalty_grid <- c(0.01, 0.02, 0.03)

  capture_calls <- function(mode, seed, n_starts = 0L) {
    calls <- list()
    fake_deconvolve_once <- function(Q, log_P, support, target_mean, penalty_value,
                                     alpha_init = NULL, seed = 1234L,
                                     n_random_starts = 0L,
                                     optimizer = "L-BFGS-B") {
      calls[[length(calls) + 1L]] <<- list(
        penalty_value = penalty_value,
        alpha_init = alpha_init,
        seed = seed,
        n_random_starts = n_random_starts,
        optimizer = optimizer
      )

      list(
        alpha = c(0, penalty_value, penalty_value + 1),
        alpha_free = c(penalty_value, penalty_value + 1),
        g = c(0.2, 0.3, 0.5),
        penalty_value = penalty_value,
        objective = penalty_value,
        convergence = 0L,
        method = "mock"
      )
    }

    testthat::local_mocked_bindings(
      .eb_deconvolve_once = fake_deconvolve_once,
      .package = "ebrecipe"
    )

    if (identical(mode, "replication") && n_starts > 0L) {
      testthat::expect_warning(
        ebrecipe:::.eb_select_penalty(
          target_var = 0.5,
          theta_hat = 0,
          s = 1,
          Q = Q,
          support = support,
          target_mean = 0,
          penalty_grid = penalty_grid,
          log_P = log_P,
          mode = mode,
          seed = seed,
          n_starts = n_starts
        ),
        "ignored"
      )
    } else {
      ebrecipe:::.eb_select_penalty(
        target_var = 0.5,
        theta_hat = 0,
        s = 1,
        Q = Q,
        support = support,
        target_mean = 0,
        penalty_grid = penalty_grid,
        log_P = log_P,
        mode = mode,
        seed = seed,
        n_starts = n_starts
      )
    }

    calls
  }

  engineering_calls <- capture_calls(mode = "engineering", seed = 500L, n_starts = 2L)
  testthat::expect_equal(engineering_calls[[1]]$seed, 500L)
  testthat::expect_null(engineering_calls[[1]]$alpha_init)
  testthat::expect_equal(engineering_calls[[1]]$n_random_starts, 2L)
  testthat::expect_identical(engineering_calls[[1]]$optimizer, "L-BFGS-B")
  testthat::expect_equal(engineering_calls[[2]]$seed, 501L)
  testthat::expect_equal(engineering_calls[[2]]$alpha_init, c(0.01, 1.01))
  testthat::expect_equal(engineering_calls[[2]]$n_random_starts, 2L)
  testthat::expect_identical(engineering_calls[[2]]$optimizer, "L-BFGS-B")

  replication_calls_1 <- capture_calls(mode = "replication", seed = 500L, n_starts = 2L)
  replication_calls_2 <- capture_calls(mode = "replication", seed = 500L, n_starts = 2L)
  replication_calls_3 <- capture_calls(mode = "replication", seed = 501L, n_starts = 2L)

  testthat::expect_true(all(vapply(replication_calls_1, function(x) is.null(x$seed), logical(1))))
  testthat::expect_true(all(vapply(replication_calls_1, function(x) identical(x$n_random_starts, 0L), logical(1))))
  testthat::expect_true(all(vapply(replication_calls_1, function(x) is.numeric(x$alpha_init), logical(1))))
  testthat::expect_true(all(vapply(replication_calls_1, function(x) identical(x$optimizer, "L-BFGS-B"), logical(1))))
  testthat::expect_false(isTRUE(all.equal(replication_calls_1[[2]]$alpha_init, c(0.01, 1.01))))
  testthat::expect_equal(
    lapply(replication_calls_1, function(x) x$alpha_init),
    lapply(replication_calls_2, function(x) x$alpha_init)
  )
  testthat::expect_false(identical(
    lapply(replication_calls_1, function(x) x$alpha_init),
    lapply(replication_calls_3, function(x) x$alpha_init)
  ))
})

# Targets: P2-PEN-001
testthat::test_that("replication penalty search is deterministic on the Walters white fixture", {
  fixture <- .step31_discrimination_fixture("white")
  support <- fixture$support
  Q <- ebrecipe:::.eb_spline_basis(support, n_knots = 5L)
  penalty_grid <- seq(0.001, 0.15, by = 0.001)
  target_var <- ebrecipe:::.eb_bias_corrected_variance(fixture$r, fixture$s_r^2)

  selection_1 <- ebrecipe:::.eb_select_penalty(
    target_var = target_var,
    theta_hat = fixture$r,
    s = fixture$s_r,
    Q = Q,
    support = support,
    target_mean = fixture$target_mean,
    penalty_grid = penalty_grid,
    mode = "replication",
    seed = 1234L,
    optimizer = "L-BFGS-B"
  )
  selection_2 <- ebrecipe:::.eb_select_penalty(
    target_var = target_var,
    theta_hat = fixture$r,
    s = fixture$s_r,
    Q = Q,
    support = support,
    target_mean = fixture$target_mean,
    penalty_grid = penalty_grid,
    mode = "replication",
    seed = 1234L,
    optimizer = "L-BFGS-B"
  )

  testthat::expect_equal(selection_1$penalty, 0.115)
  testthat::expect_equal(selection_1$penalty, selection_2$penalty)
  testthat::expect_equal(selection_1$criterion, selection_2$criterion, tolerance = 1e-12)
  testthat::expect_equal(
    selection_1$all_results$criterion,
    selection_2$all_results$criterion,
    tolerance = 1e-12
  )
  testthat::expect_true(selection_1$penalty %in% penalty_grid)
  testthat::expect_identical(selection_1$method, "L-BFGS-B")
  testthat::expect_lte(
    abs(sqrt(selection_1$fitted_var) - sqrt(selection_1$target_var)),
    0.10 * sqrt(selection_1$target_var) + 1e-8
  )
})
