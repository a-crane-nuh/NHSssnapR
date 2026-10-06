#' Renames columns to align datasets 5.0.0 to 6.0.0
#'
#' @param data the df that will be used. SSNAP_Export_000_Locked_YYYYMMDD_YYYYMMDD is the 5.0.0 format.
#' @param rename_map the from and to columns. e.g. from S1Surname to S01Surname. note the addition of a zero.
#'
#'
#' @returns A new column name.
#' @export
#'
#' @examples
#' x <- data.frame(value_one = c("a"),value_two = c(1),value_three = c("alpha") )
#' rename_ssnap_columns(x,c(value_one = "value1",value_two = "value2",value_three = "value3"))
rename_ssnap_columns <- function(data, rename_map) {

  stopifnot(is.data.frame(data))
  stopifnot(is.character(rename_map))
  stopifnot(!is.null(names(rename_map)))

  old_name <- names(rename_map)
  new_name <- unname(rename_map)

  data.table::setnames(
    data
    ,old = old_name
    ,new = new_name
    ,skip_absent = TRUE
  )

  missing_cols <- setdiff(new_name, names(data))

  for (col in missing_cols) {
    data[[col]] <- NA
  }

  out <- data[, new_name, drop = FALSE]

  out
}
