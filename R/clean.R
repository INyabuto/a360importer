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

  for (col in names(dt)) {
    set(dt, i = which(is.na(dt[[col]])), j = col, value = "")
  }

}




