#' Transform elements
#'

# baseurl <- "https://data.psi-mis.org/"
# baseurl2 <- "https://staging.psi-mis.org/"
# base <- substr(baseurl,9,24)
#
# base2 <- substr(baseurl2,9,27)
#
# pwd <- keyringr::decrypt_kc_pw(base, type = "internet")
# #pwd2 <- keyringr::decrypt_kc_pw(base2, type = "internet")
# usr <- "inyambuka"
#
# loginDHIS2(baseurl,usr,pwd)
# # # dt names
# #
# dt_cols <- dt_names(d)
# #
# write.csv(as.data.frame(dt_cols), row.names = F, file = "./inst/extdata/data cols.csv")
# #
#
# write.csv(prog_des_9ja()$content, row.names = F, file = "./inst/extdata/9ja program des.csv")
# write.csv(prog_des_prov()$content, row.names = F, file = "./inst/extdata/provider program des.csv")
#


#' Transform sheets
#'
#' \code{transform_sheets} transforms attendance sheets to a standard format.
#'
#' @param dt A list of data.table or a data.frames
#' @return A list of transformed sheets.
transform_sheets <- function(dt){
  dt <- dt %>%
    dplyr::select(-c("State","Region","LGA","Ward","Type of Facility","warn_missing"))

  # remap the col names
  names(dt) <- plyr::mapvalues(names(dt),
                               from = meta$dt_cols,
                               to = meta$id,
                               warn_missing = F)
  dt
  #

}



#' Transform eventDate
#'
#' format dates to DHIS2 stardard event dates
#' @param dt A data.table or list of data.frames
#' @return A tranformed list
transform_date <- function(dt){
  dt %>% dplyr::mutate(eventDate = event_dates(.$eventDate))
}


#' Event dates
#'
#' Convert elements dates to standard DHIS2 eventDates
#'
#' @param x An vector or character string
#' @return DHIS2 eventDate
event_dates <- function(x){

  if (!is.na(x) && grepl("/", x)){
    x %>% as.character() %>%
      strptime(., "%d/%m/%Y") %>%
      format("%Y-%m-%d")
    # strptime(as.character(x),"%d/%m/%Y") %>%
    #   format("%Y-%m-%d")
  }else if (!is.na(x) && grepl("-", x)){
    as.Date(x, format = "%d-%b-%y") %>%
      format("%Y-%m-%d")

  }
}





