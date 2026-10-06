#' parse all date, datetime fields to POSIXct format
#'
#' @param df  data.frame imported from SSNAP as a raw csv file using
#'    data.table::fread
#'
#' @returns The imported dataframe with date fields formatted from
#'    character to POSIXct
#'
#'
#' @examples
#' \dontrun{
#' imported_ssnap_data <- parse_datetime_columns(imported_ssnap_data)
#' }
parse_datetime_columns <- function(df) {

  find_and_format <- function(x) {

    if (!is.character(x) && !is.factor(x)) return(x)

    x <- trimws(as.character(x))
    x[x == ""] <- NA

    formats <- c(
      "%d/%m/%Y %H:%M:%S",
      "%d/%m/%Y %H:%M",
      "%d/%m/%Y"
    )

    out <- rep(as.POSIXct(NA), length(x))

    for (fmt in formats) {
      missing <- is.na(out) & !is.na(x)
      out[missing] <- as.POSIXct(x[missing], format = fmt, tz = "Europe/London")
    }

    # If nothing converted → return original
    if (all(is.na(out))) return(x)

    out
  }

  as.data.frame(lapply(df, find_and_format))
}
