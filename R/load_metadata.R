#' Load metadata
#'    loads files used in aggregation and plot creation
#'
#' @param file filename of metadata
#' @param skip sip a row on import, default is zero
#'
#' @returns data.frames
#' @export
#'
#' @examples
#' \dontrun{
#' load_metadata("filenames.csv")
#' }
load_metadata <- function(file, skip = 0) {

  path <- system.file("extdata"
                      , file
                      , package = "NHSssnapR")

  if (path == "") {
    stop("Metadata file not found: ", file)
  }

  ref <- data.table::fread(path, skip = skip)

  # global cleaning (applied to all metadata)
  ref[] <- lapply(ref, function(x) {
    if (is.character(x)) trimws(x) else x
  })

  ref
}
