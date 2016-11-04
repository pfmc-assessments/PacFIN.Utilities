context("getExpansion_2 DBRK")

test_that("Test getExpansion_2 on DBRK from Vlada.", {
  d <- system.file("extdata", package = "ss3sim")
  catchfn <- file.path(d, "Catch.DBRK.csv")
  catch <- read.csv(catchfn, skip = 1, as.is = TRUE)
  # Rename the catch columns
  colnames(catch) <- c("Year", "CA.TWL", "OR.TWL", "WA.TWL", "CA.NONTWL",
    "OR.NONTWL", "WA.NONTWL")
  # No WA.NONTWL in example Pdata (from PTRL),
  # remove column 7 for this example to run.
  catch <- catch[, -7]

  # Encode the two gears to use in this example
  # Note that the state value will be pasted to the usegear to create the full name.
  testdat <- Pdata
  testdat$usegear = "NONTWL"
  testdat$usegear[testdat$geargroup %in% c("TWL","TWS")] = "TWL"

  # Get the expansion, converting the metric tonnes of catch to pounds
  testdat <- getExpansion_2(testdat, catch, Convert = TRUE, maxExp = 0.9)

  expect_error({
    # code example here
  }
  )
})
