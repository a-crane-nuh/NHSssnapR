test_that("import and remap, has the correct number of columns", {

  mock_data <- data.frame(
    S1Forename = c("John","Motoko","Paul","Malcolm","Ellen")
    ,S1Surname = c("Sheridan","Kusanagi","Atreides","Reynolds","Ripley")
    ,S1BirthDate = c("2215-05-12","1991-01-01","1017-01-01","2468-09-20","2092-01-07")
    ,S1AgeOnArrival = c(1,2,3,4,5)
    ,S1Gender = c("M","U","M","M","F")
    ,S1PostcodeOut = c("NG1","NG2","NG3","NG4","NG5")
    ,S1Diagnosis = c("S","O","S","O","S")
    ,S1OnsetDateTime = c("2026-01-02 01:02","2026-02-03 01:02","2026-04-05 01:02","2026-06-07 01:02","2026-08-09 01:02")
    ,S1ArriveByAmbulance = c("N","Y","Y","Y","Y")
    ,S1FirstArrivalDateTime = c("2026-01-02 01:02","2026-02-03 01:02","2026-04-05 01:02","2026-06-07 01:02","2026-08-09 01:02")
  )

  file_path <- tempfile(fileext = ".csv")

  write.csv(mock_data, file_path, row.names = FALSE)

  import_test <- import_dataset_5_0_0(dirname(file_path)
                                      ,basename(file_path))

  expect_equal(length(import_test), 417)

})

test_that("data is expected", {

  mock_data <- data.frame(
    S1Forename = c("John","Motoko","Paul","Malcolm","Ellen")
    ,S1Surname = c("Sheridan","Kusanagi","Atreides","Reynolds","Ripley")
    ,S1BirthDate = c("2215-05-12","1991-01-01","1017-01-01","2468-09-20","2092-01-07")
    ,S1AgeOnArrival = c(1,2,3,4,5)
    ,S1Gender = c("M","U","M","M","F")
    ,S1PostcodeOut = c("NG1","NG2","NG3","NG4","NG5")
    ,S1Diagnosis = c("S","O","S","O","S")
    ,S1OnsetDateTime = c("2026-01-02 01:02","2026-02-03 01:02","2026-04-05 01:02","2026-06-07 01:02","2026-08-09 01:02")
    ,S1ArriveByAmbulance = c("N","Y","Y","Y","Y")
    ,S1FirstArrivalDateTime = c("2026-01-02 01:02","2026-02-03 01:02","2026-04-05 01:02","2026-06-07 01:02","2026-08-09 01:02")
  )

  file_path <- tempfile(fileext = ".csv")

  write.csv(mock_data, file_path, row.names = FALSE)

  import_test <- import_dataset_5_0_0(
    dirname(file_path),
    basename(file_path)
  )
  # expect data has not been lost
  expect_true(all(!is.na(import_test$S01Forename)))
})


test_that("data is NOT expected and all NA", {
  # some mock data that are present in the 5.0.0 dataset
  mock_data <- data.frame(
    S1Forename = c("John","Motoko","Paul","Malcolm","Ellen")
    ,S1Surname = c("Sheridan","Kusanagi","Atreides","Reynolds","Ripley")
    ,S1BirthDate = c("2215-05-12","1991-01-01","1017-01-01","2468-09-20","2092-01-07")
    ,S1AgeOnArrival = c(1,2,3,4,5)
    ,S1Gender = c("M","U","M","M","F")
    ,S1PostcodeOut = c("NG1","NG2","NG3","NG4","NG5")
    ,S1Diagnosis = c("S","O","S","O","S")
    ,S1OnsetDateTime = c("2026-01-02 01:02","2026-02-03 01:02","2026-04-05 01:02","2026-06-07 01:02","2026-08-09 01:02")
    ,S1ArriveByAmbulance = c("N","Y","Y","Y","Y")
    ,S1FirstArrivalDateTime = c("2026-01-02 01:02","2026-02-03 01:02","2026-04-05 01:02","2026-06-07 01:02","2026-08-09 01:02")
  )

  file_path <- tempfile(fileext = ".csv")

  write.csv(mock_data, file_path, row.names = FALSE)

  import_test <- import_dataset_5_0_0(
    dirname(file_path),
    basename(file_path)
  )
  # expect NAs have been coerced
  expect_false(all(!is.na(import_test$S01PatientId)))
})
