test_that("Pull works for sablefish", {
  password_file_name <- "password.txt"
  skip_on_ci()
  skip_if_not(
    fs::file_exists(password_file_name),
    message = "Your password is not saved."
  )
  species <- "SABL"
  password <- readLines(password_file_name)
  bds.pacfin <- PullBDS.PacFIN(
    pacfin_species_code = species,
    password = password,
    verbose = FALSE
  )
  catch.pacfin <- PullCatch.PacFIN(
    pacfin_species_code = species,
    password = password,
    verbose = FALSE
  )
  database <- RODBC::odbcConnect(
    dsn = "PacFIN",
    uid = getUserName("PacFIN"),
    pw = password
  )
  on.exit(RODBC::odbcClose(database), add = TRUE, after = FALSE)
  landings_year <- RODBC::sqlQuery(
    database,
    glue::glue("
      SELECT LANDING_YEAR, sum(ROUND_WEIGHT_MTONS) CATCH_MT
      FROM PACFIN_MARTS.COMPREHENSIVE_FT
      WHERE PACFIN_SPECIES_CODE = 'SABL' and COUNCIL_CODE = 'P'
      GROUP BY LANDING_YEAR
      ORDER BY LANDING_YEAR
    "),
    as.is = FALSE
  )
  catch_summary <- catch.pacfin %>%
    dplyr::group_by(LANDING_YEAR) %>%
    dplyr::summarize(
      CATCH_MT = sum(ROUND_WEIGHT_MTONS)
    ) %>%
    dplyr::arrange(LANDING_YEAR)
  age_year <- RODBC::sqlQuery(
    database,
    glue::glue("
      SELECT SAMPLE_YEAR, median(FINAL_FISH_AGE_IN_YEARS) MEAN_AGE
      FROM PACFIN_MARTS.COMPREHENSIVE_BDS_COMM
      WHERE PACFIN_SPECIES_CODE = 'SABL'
      GROUP BY SAMPLE_YEAR
      ORDER BY SAMPLE_YEAR
    "),
    as.is = FALSE
  )
  bds_summary <- bds.pacfin %>%
    dplyr::group_by(SAMPLE_YEAR) %>%
    dplyr::summarize(
      MEAN_AGE = median(FINAL_FISH_AGE_IN_YEARS, na.rm = TRUE)
    ) %>%
    dplyr::arrange(SAMPLE_YEAR)

  # Tests for both data frames
  expect_true(inherits(catch.pacfin, "data.frame"))
  expect_true(inherits(bds.pacfin, "data.frame"))

  # Tests for landings
  expect_true(all.equal(as.data.frame(catch_summary), landings_year))

  # Tests for biological data
  # Have to use a tolerance of 1 because medians are calculated differently
  expect_true(all(
    abs(bds_summary %>% dplyr::pull(MEAN_AGE) - age_year[, "MEAN_AGE"]) <= 1,
    na.rm = TRUE
  ))
  expect_equal(NROW(dplyr::filter(bds.pacfin, SAMPLE_YEAR == 2008)), 12552)
})
