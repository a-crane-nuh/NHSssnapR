#' Import and cleanse ssnap data export dataset version series 6.0.0
#'
#' @param file_path the file path for where the user stores the data.
#'    the default will be the working directory
#' @param file_name_pattern the file name, this will default to "SSNAP_Core_"
#'    followed by dates as per standard SSNAP export naming convention
#'
#' @param dataset_5_0_0 optional if user is also loading in the older dataset
#'    5.0.0 series
#'
#' @returns a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' import_and_cleanse("file/path","file_name")
#' import_and_cleanse("this/dir/path","my renamed SSNAP file")
#' }
import_and_cleanse <- function(file_path = getwd()
                               ,file_name_pattern = file_name_pattern
                               ,dataset_5_0_0 = NULL
                               ) {

  dataset_6_0_0 <- data.table::fread(
    dir( file_path##
         ,full.names = TRUE
         ,pattern = file_name_pattern##"^SSNAP_Core_"
    )
  )
# ---- time to check if the user created a dataset 5 version and followed instructions ----

  if (!is.null(dataset_5_0_0) &&
      identical(names(dataset_5_0_0), names(dataset_6_0_0)))
  {

  imported_ssnap_data <- rbind(dataset_5_0_0,dataset_6_0_0)

  } else {

    imported_ssnap_data <- dataset_6_0_0
  }

if ( exists("imported_ssnap_data") ) {

  # ---- start data cleanse  ----
  ## ---- date format and apply (refer to parse_import_dataframe for details)----

  imported_ssnap_data <- parse_datetime_columns(imported_ssnap_data)
  ## ---- format date end ----

  # ---- find and replace logical and psuedo logical to binary ----
  logical <- names(imported_ssnap_data)[sapply(imported_ssnap_data, is.logical)]

  imported_ssnap_data[logical] <- lapply(imported_ssnap_data[logical], function(x) as.integer(as.logical(x)))

  remaining_cols <- names(imported_ssnap_data)[vapply(imported_ssnap_data, function(x) {

  if (!is.character(x) ) return(FALSE)
  if ( all(is.na(x)) ) return(FALSE)
  if ( is.logical(x)) return(FALSE)

  clean_x <- toupper(trimws(x))
  clean_x[clean_x ==""] <- NA_character_

  x_non_na <- unique(stats::na.omit(clean_x))

  # Skip TRUE/FALSE-only columns (already handled)
  if (length(x_non_na) > 0 && all(x_non_na %in% c("TRUE", "FALSE"))) {
    return(FALSE)
  }

  TRUE

  }, logical(1))]

  pseudo_cols <- names(imported_ssnap_data[remaining_cols])[
    sapply(imported_ssnap_data[remaining_cols], function(x) {

      if (!is.character(x)) return(FALSE)

      clean_x <- toupper(trimws(x))
      clean_x[clean_x == ""] <- NA_character_

      x_non_na <- unique(stats::na.omit(clean_x))

      length(x_non_na) > 0 && all(x_non_na %in% c("Y", "N","YES", "NO"))

    })
  ]

  imported_ssnap_data[pseudo_cols] <- lapply(
    imported_ssnap_data[pseudo_cols],
    function(x) {
      x_clean <- toupper(trimws(x))
      x_clean[x_clean == ""] <- NA_character_

      out <- rep(NA_integer_, length(x_clean))

      out[x_clean == "Y"] <- 1L
      out[x_clean == "N"] <- 0L

      out
    }
  )
  # lapply(imported_ssnap_data[, pseudo_cols, drop = FALSE], unique) # for testthat
  # ---- logical end ----
  # ---- convert to numeric where applicable  ----
  nihss_unknown <- grep("^S02Nihss",names(imported_ssnap_data), value = TRUE)
  imported_ssnap_data[nihss_unknown]<-lapply(imported_ssnap_data[nihss_unknown],function(x)  ifelse(x==-1,NA,x))
  imported_ssnap_data[nihss_unknown] <- lapply(imported_ssnap_data[nihss_unknown],function(x) suppressWarnings(as.numeric(x)))
  therapy_minutes <- grep("Minutes$",names(imported_ssnap_data), value = TRUE)
  imported_ssnap_data[therapy_minutes] <- lapply(imported_ssnap_data[therapy_minutes],function(x) suppressWarnings(as.numeric(x)))

} else {

  message("file not imported or file not recognised ssnap format")
  return(NULL)
}

return(imported_ssnap_data)
}
