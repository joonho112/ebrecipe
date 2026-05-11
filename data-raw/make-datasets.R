find_package_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0L) {
    script_path <- sub("^--file=", "", file_arg[[1L]])
    script_path <- gsub("~\\+~", " ", script_path)
    script_path <- normalizePath(script_path)
    return(dirname(dirname(script_path)))
  }

  wd <- normalizePath(getwd())
  if (basename(wd) == "data-raw") {
    return(dirname(wd))
  }

  wd
}

copy_fixture <- function(src, dest) {
  ok <- file.copy(src, dest, overwrite = TRUE)
  if (!ok) {
    stop(sprintf("Failed to copy fixture: %s", basename(src)), call. = FALSE)
  }
}

pkg_root <- find_package_root()
disc_dir <- file.path(pkg_root, "data-raw", "discrimination")
vam_dir <- file.path(pkg_root, "data-raw", "school_vam")
data_dir <- file.path(pkg_root, "data")
fixture_dir <- file.path(pkg_root, "tests", "testthat", "fixtures")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

krw_summary <- read.csv(
  file.path(disc_dir, "krw_firm_summary.csv"),
  stringsAsFactors = FALSE
)

krw_firms <- data.frame(
  firm_id = krw_summary$firm_id,
  theta_hat_race = krw_summary$theta_white,
  se_race = krw_summary$s_white,
  theta_hat_gender = krw_summary$theta_male,
  se_gender = krw_summary$s_male,
  stringsAsFactors = FALSE
)

attr(krw_firms, "sample_stats") <- list(
  full_observations = 83643L,
  full_firms = 108L,
  dropped_observations = 4733L,
  filtered_firms = 97L,
  filtered_observations = 78910L
)

vam_ests_raw <- read.csv(
  file.path(vam_dir, "VAM_ests_m3_test2_year2014.csv"),
  check.names = FALSE
)
vam_vce_raw <- as.matrix(
  read.csv(
    file.path(vam_dir, "VAM_vce_m3_test2_year2014.csv"),
    check.names = FALSE
  )
)
vam_sectors_raw <- read.csv(
  file.path(vam_dir, "VAM_sects_long.csv"),
  stringsAsFactors = FALSE
)

theta_hat <- as.numeric(vam_ests_raw[1L, ])
se <- sqrt(diag(vam_vce_raw))
charter <- as.integer(vam_sectors_raw[[1L]])

vam_schools <- data.frame(
  school_id = seq_along(theta_hat),
  theta_hat = theta_hat,
  se = se,
  charter = charter == 1L
)

vam_simulated_raw <- read.csv(
  file.path(vam_dir, "simulated_student_data.csv"),
  stringsAsFactors = FALSE
)

vam_simulated <- data.frame(
  student_id = seq_len(nrow(vam_simulated_raw)),
  school_id = as.integer(vam_simulated_raw$D),
  x = vam_simulated_raw$X,
  theta_true = vam_simulated_raw$theta_D,
  y = vam_simulated_raw$Y
)

save(
  krw_firms,
  file = file.path(data_dir, "krw_firms.rda"),
  compress = "bzip2"
)
save(
  vam_schools,
  file = file.path(data_dir, "vam_schools.rda"),
  compress = "bzip2"
)
save(
  vam_simulated,
  file = file.path(data_dir, "vam_simulated.rda"),
  compress = "bzip2"
)

for (name in c(
  "posteriors_white.csv",
  "posteriors_male.csv",
  "g_r_white.csv",
  "g_r_male.csv",
  "g_theta_white.csv",
  "g_theta_male.csv",
  "posterior_grid_white.csv",
  "posterior_grid_male.csv",
  "spline_info.csv",
  "estimates_white.csv",
  "estimates_male.csv"
)) {
  copy_fixture(file.path(disc_dir, name), file.path(fixture_dir, name))
}

copy_fixture(
  file.path(disc_dir, "Q.csv"),
  file.path(fixture_dir, "Q_matlab.csv")
)
copy_fixture(
  file.path(disc_dir, "step2_2_sd_estimates_results.csv"),
  file.path(fixture_dir, "step2_2_sd_estimates_results.csv")
)

krw_firm_summary_fixture <- krw_summary[
  c("firm_id", "j", "n_j", "p", "theta_white", "s_white", "theta_male", "s_male")
]
krw_firm_summary_fixture$v_white <- krw_firm_summary_fixture$s_white^2
krw_firm_summary_fixture$v_male <- krw_firm_summary_fixture$s_male^2
krw_firm_summary_fixture <- krw_firm_summary_fixture[
  c(
    "firm_id", "j", "n_j", "p",
    "theta_white", "s_white", "v_white",
    "theta_male", "s_male", "v_male"
  )
]
write.csv(
  krw_firm_summary_fixture,
  file.path(fixture_dir, "krw_firm_summary.csv"),
  row.names = FALSE
)

vam_ests_fixture <- data.frame(
  school_id = seq_along(theta_hat),
  theta_hat = theta_hat,
  se = se,
  charter = charter,
  sector = ifelse(charter == 1L, "charter", "non_charter"),
  stringsAsFactors = FALSE
)
write.csv(vam_ests_fixture, file.path(fixture_dir, "vam_ests.csv"), row.names = FALSE)
write.csv(
  data.frame(
    school_id = seq_along(theta_hat),
    charter = charter
  ),
  file.path(fixture_dir, "vam_sectors.csv"),
  row.names = FALSE
)
write.csv(vam_vce_raw, file.path(fixture_dir, "vam_vce.csv"), row.names = FALSE)

simulation_summary_fixture <- data.frame(
  N = nrow(vam_simulated),
  J = nrow(vam_schools),
  mean_Y = mean(vam_simulated$y),
  sd_Y = stats::sd(vam_simulated$y),
  mean_X = mean(vam_simulated$x),
  sd_X = stats::sd(vam_simulated$x),
  mean_theta = mean(vam_simulated$theta_true),
  sd_theta = stats::sd(vam_simulated$theta_true),
  charter_share = mean(as.integer(vam_schools$charter)),
  seed = 20240101L
)
write.csv(
  simulation_summary_fixture,
  file.path(fixture_dir, "simulation_summary.csv"),
  row.names = FALSE
)

message("Created user datasets:")
message("  - ", file.path(data_dir, "krw_firms.rda"))
message("  - ", file.path(data_dir, "vam_schools.rda"))
message("  - ", file.path(data_dir, "vam_simulated.rda"))
message("Created fixtures in: ", fixture_dir)
