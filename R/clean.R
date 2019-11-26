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

#' Select Colums to Transform
#'
#' \code{select_att} selects columns names and types to transform from the attendance sheets.
#'
#' @param dt a data.table or a list of data.frames
#' @return a cleaned dataset
select_att <- function(dt = NULL, cols = NULL){

  if (!is.null(dt) && is.null(cols)){

    cols <- c("State","Region","LGA","Ward","Facility","Name","ID #", "Age","Date Calculation","Program Activity")

    dplyr::select(dt,cols)
  }

  if (!is.null(dt) && !is.null(cols)){
    dplyr::select(dt, cols)
  }



}


#' Select Columns to Transform
#'
#' \code{select_prov} select column names and types to transform from the serv ice provison sheets.
select_prov <- function(dt = NULL, cols = NULL){

  if (!is.null(dt) && is.null(cols)){
    cols <- c("State","Region","LGA","Ward","Facility","ID #", "Age (Formula)","Date Calculation","Program Entry Point",
              "Visit Type","Rec. Counseling","Current Method","Used EC/Condoms last sex","Signed assent/Consent (All)","15-17 Given Consent Form",
              "15-17 Returned Consent Form", "Pregnant?", "Method Received","Received Condoms as a dual Method", "Currently Using","Took up Method", "Provider Outcome Status", "STI Treatment","Date of treatment",
              "Case Treated","Drugs Given", "QTY of Drugs Given")
    dplyr::select(dt,cols)
  }

  if (!is.null(dt) && !is.null(cols)){
    dplyr::select(dt,cols)
  }
}



#' Get data frame names
#'
#' @param dt a list of data tables
#' @return dt names
dt_names <- function(dt){

  # get the names of the files
  if (length(dt) > 0) {

    dt_names <- sapply(dt, names)

    dt_names_c <- do.call(c, dt_names)

    dt_names <- unique(dt_names_c)

    names(dt_names) <- dt_names

    dt_names
  }

}


