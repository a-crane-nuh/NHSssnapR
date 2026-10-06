test_that("derive_num_denom_flags validates input", {

  path <- testthat::test_path("data/")

  df <- NHSssnapR::import_and_cleanse(path,"^synthe")

  expect_error(derive_num_denom_flags(NULL), "Oops, did you forget to add a df")
  expect_error(derive_num_denom_flags("x"), "input must be a data.frame")
})

test_that("binary indicator columns only contain 0, 1, or NA", {

  path <- testthat::test_path("data/")

  df <- NHSssnapR::import_and_cleanse(path,"^synthe")

  result <- derive_num_denom_flags(df)

  binary_cols <- c(
    "dc"
    ,"KI_1_1n", "KI_1_3n", "KI_1_4d", "KI_1_4n", "KI_1_5n", "KI_1_6n", "KI_1_7d", "KI_1_7n"
    ,"KI_2_1n", "KI_2_1d", "KI_2_3n", "KI_2_3d","KI_2_4n", "KI_2_4d"
    ,"KI_3_1n", "KI_3_2n", "KI_3_2d", "KI_3_3n", "KI_3_3d","KI_3_5n"
    ,"KI_4_1n", "KI_4_2n", "KI_4_2d", "KI_4_3n", "KI_4_3d","KI_4_4n", "KI_4_4d", "KI_4_5n", "KI_4_5d"
    ,"KI_5_2n", "KI_5_2d"
    ,"KI_6_1d", "KI_6_1n", "KI_6_3d", "KI_6_3n","KI_6_5d", "KI_6_5n"
    ,"KI_7_1n", "KI_7_1d", "KI_7_2n", "KI_7_3n", "KI_7_3d","KI_7_4n", "KI_7_4d"
    ,"KI_7_5n", "KI_7_5d","KI_7_6n", "KI_7_6d", "KI_7_7n", "KI_7_7d","KI_7_8n", "KI_7_8d"
  )

  for (col in binary_cols) {
    expect_true(all(result[[col]] %in% c(0L, 1L, NA)))
  }
})

test_that("DomainOne basic logic works", {

  path <- testthat::test_path("data/")

  df <- NHSssnapR::import_and_cleanse(path,"^synthe")

  result <- derive_num_denom_flags(df)

  expect_true(all(result$d == 1))
  expect_equal(result$dc, as.integer(!is.na(df$S07DepartureDate)))
  # expected value output, NA not summed in aggregation
  expect_true(all(result$KI_1_1n %in% c(0, 1, NA)))
  # expected in 1/0 output
  expect_type(result$KI_1_1n, "integer")
  # expecting in duration
  expect_type(result$KI_1_2n, "double")
  # therapy window used in
  TherapyWindow = ceiling(pmax(as.numeric(
    difftime(
      pmin(
        as.Date(df$S07EndRehabDate, tz = "Europe/London"),
        as.Date(df$S07DepartureDate, tz = "Europe/London"),
        na.rm = TRUE
      ),
      as.Date(df$S01ClockStartTime, tz = "Europe/London"),
      unit = "days"
    )
  ) - 1, 1))

  # total therapy minutes used in KI_5_2n, KI_5_2d, KI_5_3n, KI_6_2n, KI_6_4n, KI_6_6n
  TotalTherapyMinutes = rowSums(
    cbind(
      df$S04CommsSwallowMinutes,
      df$S04MotorMinutes,
      df$S04PsychoFunctionMinutes,
      df$S04OtherMinutes
    ),
    na.rm = TRUE
  )
  # total sum
  expect_equal(sum(TotalTherapyMinutes), 23040)

  # d 6 tests
  # unique in KI_6_2n, KI_6_4n, KI_6_6n

  KI_6_2n <-df$S04MotorDays/TherapyWindow

  expect_equal(result$KI_6_2n, KI_6_2n)

  # following unique to KI_7_3n, KI_7_4n, KI_7_5n,   KI_7_6n
  expect_type(ifelse(is.na(result$S06MoodScreeningDate), 0, 1),"logical")

})
