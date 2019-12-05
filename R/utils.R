#' Remove empty rows
#'
#' \code{remove_empty_rows} finds rows with entire NAs and and wipes off.
#'
#' @param df A data frame
#' @return a packed df
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
remove_nas <- function(dt){

  dt[is.na(dt)] <- ""

}



#' GET optionSet details of a data element
#'
#' @param uid A dataElement uid
#' @return an S3 object with the optsionSet id, options, path and the response.
de_optionset <- function(uid = NULL){

  ua <- user_agent("https://github.com/INyabuto/a360importer")



  if (!is.null(id)){

    path = paste0("api/dataElements/",uid)

    url <- modify_url("https://data.psi-mis.org", path = path, query = paste0("fields=optionSet[id]"))

    resp <- GET(url, ua)

  }

  if (http_type(resp) != "application/json"){
    stop("PSI MIS API did not return a json", call. = FALSE)
  }

  parsed <- fromJSON(content(resp,"text"))

  # Turn HTTP errors into R

  if (http_error(resp)){
    stop(sprintf("PSI - MIS API request failed with status [%s]\n%s\n<%s>",
                 status_code(resp),
                 parsed$message,
                 "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"),
         call. = FALSE)
  }


  structure(
    list(optionSet_id = parsed$optionSet$id,
         options = de_options(parsed$optionSet$id)$content$options,
         path = path,
         response = resp),
    class = "de_optionset_id"
  )

}


print.de_optionset <- function(x,...){
  cat("[PSI - MIS ", x$path, "]\n", sep = "")
  str(x)
  invisible(x)
}



#' GET options in a optionSet
#'
#' @param uid An optionSet uid
#' @return an S3 object with the content, path and the response.
de_options <- function(uid = NULL){

  ua <- user_agent("https://github.com/INyabuto/a360importer")

  if (!is.null(uid)){

    path = paste0("api/optionSets/",uid)

    url <- modify_url("https://data.psi-mis.org", path = path, query = paste0("fields=options[id,code,name]"))

    resp <- GET(url, ua)

  }

  if (http_type(resp) != "application/json"){
    stop("PSI MIS API did not return a json", call. = FALSE)
  }

  parsed <- fromJSON(content(resp,"text"))


  # Turn HTTP errors into R

  if (http_error(resp)){
    stop(sprintf("PSI - MIS API request failed with status [%s]\n%s\n<%s>",
                    status_code(resp),
                    parsed$message,
                    "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"),
            call. = FALSE)
  }

  structure(
    list(content = parsed,
         path = path,
         response = resp),
    class = "de_options"
    )

}

#' Retrive Data element uid
#'@param x A vector of character string
de_uid <- function(x){

  index <- str_which(meta$dhis2, coll(str_trim(x), ignore_case = T))

  meta$id[index]
}
