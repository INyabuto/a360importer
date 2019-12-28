
#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
#' @noRd
check_internet <- function(){
  stop_if_not(.x = has_internet(),
              msg = "Please check your internet connection")

}

#' @importFrom httr http_error status_code
#' @noRd
check_status <- function(resp, parsed){
  if (http_error(resp)) {
    stop(
      sprintf(
        "PSI - MIS API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"
      ),
      call. = FALSE
    )
  }

}

#' @importFrom httr http_error status_code
#' @noRd
check_status_on_upload <- function(resp, parsed){
  if (http_error(resp)){
    warning(
      sprintf("PSI - MIS API request failed [%s]\n%s\n<%s>",
              status_code(resp),
              error_description(parsed),
              "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"),
      call. = F
    )

  }
}

#' error decription on upload
error_description <- function(x){
  x$response$importSummaries$description %>%
    unique(.) %>%
    .[!is.na(.)] %>%
    paste0(., collapse = "")
}


#' @importFrom httr http_type
#' @noRd
check_content <- function(resp){
  if (http_type(resp) != "application/json"){
    stop("PSI - MIS API did not return json", call. = FALSE)
  }
}

#' @noRd
api_version <- function(version = NULL){

  if (!is.null(version)){
    version <- version
  }

  if (is.null(version)){
    version <- 29
  }

  version
}

#' @importFrom httr user_agent
#' @noRd
set_agent <- function(agent = NULL){
  if (is.null(agent)){
    agent <- "https://github.com/INyabuto/a360importer"
    user_agent(agent)
  }

  if (!is.null(agent)){
    user_agent(agent)
  }

}


#' Remove empty rows
#'
#' \code{remove_empty_rows} finds rows with entire NAs and and wipes off.
#'
#' @param df A data frame
#' @return a packed df
#' @noRd
remove_empty_rows <- function(dt){

  # replace "" with NAs
  for(col in names(dt)) {
    set(dt, i= which(dt[[col]] == ""), j = col, value=NA)
  }

  dt[rowSums(is.na(dt)) != ncol(dt),]


}

#' Remove NAs
#'
#' @param dt a data.table
#' @return a table without NAs
#' @noRd
remove_nas <- function(dt){

  dt[is.na(dt)] <- ""

  dt

}


#' Retrive Data element uid
#' @param x A vector of character string
#' @noRd
de_uid <- function(x){

  index <- str_which(meta$dhis2, coll(str_trim(x), ignore_case = T))

  meta$id[index]
}


#' summary report
#' @noRd
summarize_files <- function(dt){
  dt <- data.frame(file = names(dt),
                rows = sapply(dt, nrow),
                stringsAsFactors = F)
  rownames(dt) <- NULL
  dt
}
