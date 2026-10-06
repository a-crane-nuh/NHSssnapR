# tests/testthat/test-plot_key_indicators.R

test_that("plot_key_indicators errors on NULL input", {

  expect_error(
    plot_key_indicators(NULL),
    "Oops, did you forget to add a file"
  )

})

test_that("plot_key_indicators errors on non-data.frame input", {

  expect_error(
    plot_key_indicators("hello"),
    "input must be a data.frame"
  )

})

test_that("plot_key_indicators creates output directory", {

  old_wd <- getwd()
  tmp_dir <- tempdir()

  setwd(tmp_dir)

  on.exit({
    setwd(old_wd)
  })

  df <- data.frame(
    date = seq.Date(
      as.Date("2024-01-01"),
      by = "month",
      length.out = 18
    ),
    Interval = "months",
    SiteName = "Test Site",
    SiteCode = "ABC",
    KI_1_1 = runif(18),
    TotalKeyIndicatorAvgScore = runif(18)
  )

  plot_key_indicators(df)

  expect_true(
    dir.exists(
      file.path(
        tmp_dir,
        "out",
        "plots",
        "key_indicators"
      )
    )
  )

})


test_that("missing key indicators are skipped without error", {

  df <- data.frame(
    date = seq.Date(
      as.Date("2024-01-01"),
      by = "month",
      length.out = 15
    ),
    Interval = "months",
    SiteName = "Test Site",
    SiteCode = "ABC"
  )

  expect_no_error(
    plot_key_indicators(df)
  )

})


test_that("all NA indicators are skipped", {

  df <- data.frame(
    date = seq.Date(
      as.Date("2024-01-01"),
      by = "month",
      length.out = 15
    ),
    Interval = "months",
    SiteName = "Test Site",
    SiteCode = "ABC",
    KI_1_1 = NA_real_
  )

  expect_no_error(
    plot_key_indicators(df)
  )

})

test_that("svg files are created", {

  old_wd <- getwd()
  tmp_dir <- tempdir()

  setwd(tmp_dir)

  on.exit({
    setwd(old_wd)
  })

  df <- data.frame(
    date = seq.Date(
      as.Date("2024-01-01"),
      by = "month",
      length.out = 15
    ),
    Interval = "months",
    SiteName = "Test Site",
    SiteCode = "ABC",
    KI_1_1 = seq(0.1, 0.9, length.out = 15)
  )

  plot_key_indicators(df)

  svg_files <- list.files(
    file.path(
      tmp_dir,
      "out",
      "plots",
      "key_indicators"
    ),
    pattern = "\\.svg",
    full.names = TRUE
  )

  expect_gt(
    length(svg_files),
    0
  )

})

test_that("rebase_date can be supplied", {

  df <- data.frame(
    date = seq.Date(
      as.Date("2024-01-01"),
      by = "month",
      length.out = 36
    ),
    Interval = "months",
    SiteName = "Test Site",
    SiteCode = "ABC",
    KI_1_1 = runif(36)
  )

  expect_no_error(
    plot_key_indicators(
      df,
      rebase_date = as.Date("2025-06-01")
    )
  )

})
