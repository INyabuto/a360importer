#' Read multiple data files (CSV)
#'
#' @description This function reads multiple CSV files in a table format and
#' returns a list of data frames from them, with the names corresponding to the
#' file names. By default  it loads all CSV files in the specified path. You can
#' choose what files to read by specifying the name or a regular expression in
#' the pattern.
#' @details \code{load_files} is used to extract data from multiple CSV files in
#' preparation for the import. It constructs and maps different paths to the
#' \code{read_sheet} function.
#'
#' @note Files must be comma separated or in a valid CSV format.
#' @note Files must abide to the project codes and naming standards.
#' @note Path must reference to a valid directory with the project files.
#'
#'
#' @importFrom purrr map
#' @param path a character vector of full path name of the directory to read
#'   from.
#' @param pattern character string containing a regular expression (or a human
#'   readable string).
#' @param ignore.case logical. Should pattern be case-insensitive?
#' @param ... Additional argument passed to \code{\link{read_sheet}}
#' @return A list of data frame with names corresponding to the file names. If
#'   there is an error in the files, only a list with passed data frames will be
#'   returned.
#' @export
#' @examples
#' \dontrun{
#' # read all the csv files in the specified path
#' path <- "./historical files/NG A360 Master Data- Final Cleaned April & May 19"
#' data <- load_files(path)
#'
#' # load attendance csv files
#' attendance <- load_files(path, pattern = "Attendance")
#'
#' # load service provision csv files
#' sp <- load_files(path, pattern = "Service Provision")
#' }
load_files <- function(path = NULL, pattern = NULL, ignore.case = FALSE, ...){
  # is path valid?
  check_path(path)

  pattern <- "\\.csv$"

  if (!is.null(pattern)){
    pattern <- pattern
  }

  # List files in the directory and constructs file paths
  files <-  list.files(path, pattern = pattern, ignore.case = ignore.case)

  if (length(files) == 0){
    stop(sprintf("no such files exist at [%s]", sQuote(path)), call. = FALSE)
  }

  file_path <- file.path(path, files)

  data <- try(
    map(file_path, function(x) read_sheet(x, ...))
    )

  names(data) <- files

  pass <- !vapply(data, is_try_error, logical(1))

  data[pass]

}

#'@title Read data from a CSV file
#'
#'@description This function reads a single CSV file in table format and returns
#'  a data frame from it, with cases corresponding to lines and variables to
#'  fields in the file. It supports reading of files which are seperated with a
#'  comma or semi-colon.
#'
#'@details \code{read_sheet} is used in the \code{\link{load_files}} to load
#'  multiple files, you can also use it indepently to load a single file. It wraps and uses
#'  the \code{\link[readr]{read_csv}} and \code{\link[readr]{read_csv2}} under
#'  the hood to load files.
#'
#'@seealso \code{\link[readr]{read_csv}} and \code{\link[readr]{read_csv2}}
#'  functions from readr.
#'
#'@importFrom readr read_csv read_csv2
#'@param file Either a path to a file, a connection, or literal data (either a
#'  single string or a raw vector). Files ending in .gz, .bz2, .xz, or .zip will
#'  be automatically uncompressed. Files starting with http://, https://,
#'  ftp://, or ftps:// will be automatically downloaded. Remote gz files can
#'  also be automatically downloaded and decompressed. Literal data is most
#'  useful for examples and tests. It must contain at least one new line to be
#'  recognised as data (instead of a path) or be a vector of greater than length
#'  Using a value of \code{\link[readr]{clipboard()}} will read from the system
#'  clipboard.
#'@param ... Additional arguments.
#'@return A \code{\link[tibble:tibble]{tibble()}}. If there are parsing
#'  problems, a warning tells you how many, and you can retrieve the details
#'  with  \code{\link[readr:problems]{problems()}}.
#'@export
read_sheet <- function(file, ...){

  if (!grepl("\\.csv$", file)){
    stop("`file` is not a CSV type ", sQuote(file), call. = FALSE)
  }

  # read sheets
  if (is_csv(file)){
    sheet <- read_csv_file(file, ...)
  }

  if (is_csv2(file)){
    sheet <- read_csv2_file(file, ...)
  }

  sheet


}
