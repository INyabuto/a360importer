#' @name de_option_set
#' @title Data element option set
#' @description Retrieve an option set of a data element.
#'
#' @param uid A character string, the data element uid.
#'
#' @importFrom httr content GET modify_url timeout
#' @importFrom jsonlite fromJSON
#' @return an S3 type object with an optsion set id, options, path and the response.
#' @details \code{de_option_set} retrieves an option set of a data element. It checks whether the data element has
#'     an option set and extracts its details including the options. \code{de_option_set} returns the option set id, options,
#'     path and the response.
#'@export
#'@examples
#'\dontrun{
#'de_option_set(uid = "xywg23thsiw")
#'}
de_option_set <- function(uid = NULL){

  ua <- set_agent()

  if (is.null(uid)){
    stop("The data element uid must be specified", call. = FALSE)
  }

  path = paste0("api/",api_version(),"/dataElements/",uid)

  url <- modify_url("https://data.psi-mis.org", path = path, query = paste0("fields=optionSet[id]"))

  # check for internet
  check_internet()

  resp <- GET(url, ua, timeout(60))

  # assess the response
  check_content(resp)


  parsed <- fromJSON(content(resp,"text"))

  # check results
  check_status(resp, parsed)

  structure(
    list(optionSet_id = parsed$optionSet$id,
         options = de_options(parsed$optionSet$id)$content$options,
         path = path,
         response = resp),
    class = "de_option_set"
  )

}

#' print method for class de_option_set
#' @param x An S3 object.
#' @noRd
print.de_option_set <- function(x,...){
  cat("[PSI - MIS ", x$path, "]\n", sep = "")
  str(x)
  invisible(x)
}

#' @name de_options
#' @title Retrieve options in an option set
#' @param uid A character string, uid of the option set.
#' @importFrom httr content GET modify_url timeout
#' @importFrom jsonlite fromJSON
#' @return an S3 type object with content, path and the response.
#' @details \code{de_options} is used internally by de_option_set to retreive options of an option set. It selects
#'     the name, code and id of options.
de_options <- function(uid = NULL){

  ua <- set_agent()

  if (is.null(uid)){
    stop("The option set uid must be specified", call. = FALSE)
  }

  path = paste0("api/",api_version(),"/optionSets/",uid)

  url <- modify_url("https://data.psi-mis.org", path = path, query = paste0("fields=options[id,code,name]"))

  # check for internet
  check_internet()

  resp <- GET(url, ua, timeout(60))

  # assess the response
  check_content(resp)

  parsed <- fromJSON(content(resp,"text"))

  # check the results
  check_status(resp, parsed)

  structure(
    list(content = parsed,
         path = path,
         response = resp),
    class = "de_options"
  )

}
