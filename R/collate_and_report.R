#' Title
#'
#' @returns a .html file
#' @export
#'
#' @examples
#' \dontrun{
#' collate_and_report()
#' }
collate_and_report  <- function() {
# qmd templates

input_dir <- file.path(getwd(), "out", "data")

qmd_template <- system.file("templates", "NHSssnapR_Template.qmd", package = "NHSssnapR")

if (qmd_template == "") {
  stop("Quarto template not found")
}

local_template <- file.path(getwd(),"NHSssnapR_Template.qmd")

file.copy(qmd_template,local_template,overwrite = TRUE)

# css files

css_file <- system.file("templates", "styles_nuh_brand.css", package = "NHSssnapR")

if (css_file == "") {
  stop("css file not found")
}
css <- file.path(getwd(), "css")

if (!dir.exists(css)) {
  dir.create(css, recursive = TRUE, showWarnings = FALSE)
}

file.copy(css_file,css,overwrite = TRUE)

# rds data

rds_file <- system.file("data", "aggregated.rds", package = "NHSssnapR")

if (file.exists("out/data/aggregated.rds")) {
  agg_data <- readRDS("out/data/aggregated.rds")
} else {stop("RDS data not found")}

site_name <- as.character(stats::na.omit(agg_data$SiteName)[1])

date <- as.character(as.Date(max(agg_data$date)))

grouped_by <-as.character(unique(agg_data$Interval))

# run quarto

quarto::quarto_render( input = local_template
                        , execute_params = list(
                         param_data = file.path(input_dir, "aggregated.rds")
                        , param_site_name = site_name
                        , param_date = date
                        , param_grouped = grouped_by
                          )
                        )

rename_from <- "NHSssnapR_Template.html"

rename_to <- paste0(site_name,"- grouped by ",grouped_by)

file.rename(rename_from,paste0(rename_to,".html"))
}
