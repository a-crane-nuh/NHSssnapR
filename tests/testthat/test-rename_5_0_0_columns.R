test_that("Renames from and to and they match", {
  expect_equal(
    names(
      rename_ssnap_columns(
        data.frame(column_one ="value_one")
        ,c(column_one = "column1"))
    )
      ,c("column1")
      )
})
