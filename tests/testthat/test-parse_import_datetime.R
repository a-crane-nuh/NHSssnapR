mock_data <- data.frame(
  S01Forename = c("John","Motoko","Paul","Malcolm","Ellen")
  ,S01Surname = c("Sheridan","Kusanagi","Atreides","Reynolds","Ripley")
  ,S01BirthDate = c('20/08/1946','25/11/1965','16/04/1960','22/02/1966','25/06/1933')
  ,S01AgeOnArrival = c(1,2,3,4,5)
  ,S01Gender = c("M","U","M","M","F")
  ,S01IsAwesome = c(TRUE,FALSE,TRUE,FALSE,TRUE)
  ,S01Diagnosis = c("S","O","S","O","S")
  ,S01OnsetDateTime =  c('13/04/2025 14:46','14/04/2025 12:14','06/05/2025 10:02','09/05/2025 11:21','14/04/2025 13:18')
  ,S01ArriveByAmbulance = c("N","Y","Y","Y","Y")
  ,S01FirstArrivalDateTime =  c('13/04/2025 14:46:59','14/04/2025 12:14:59','06/05/2025 10:02:59','09/05/2025 11:21:59','14/04/2025 13:18:59')
)

file_path <- tempfile(fileext = ".csv")

write.csv(mock_data, file_path, row.names = FALSE)

mock_data_csv <- data.table::fread(
  dir( dirname(file_path)
       ,full.names = TRUE
       ,pattern = basename(file_path)
       )
  )

test_that("from charachter to POSIXct", {

  mock_data_csv <- parse_datetime_columns(mock_data_csv)

  expect_s3_class(mock_data_csv$S01BirthDate, "POSIXct")

  expect_s3_class(mock_data_csv$S01FirstArrivalDateTime, "POSIXct")

  expect_type(mock_data_csv$S01Forename, "character")

  expect_type(mock_data_csv$S01IsAwesome, "logical")

})
