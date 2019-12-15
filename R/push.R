#' Upload payload
#'
#' This function uploads events to PSI production server, `data.psi-mis.org`.
#'
#' \code{upload} function uploads individual events to two PSI servers; `data.psi-mis.org`
#' and `clone.psi-mis.org`. By default it sends the events to the test server available at `clone.psi-mis.org`.
#'
#'@param x A data.frame object, the event payload.
#'@param importStrategy A string specifying the action to apply on the import.It indicates whether to Save objects of all,
#' new or update import status on the server. One of 'CREATE', 'UPDATE', 'CREATE_AND_UPDATE' and 'DELETE'.The default one is
#' "CREATE".
#'@param dryRun A logical input; Save changes on the server or just return the import summary.By default, The default one is
#'FALSE, to save the changes on the server.
#'@param skipNotifications A logical input; Indicates whether to send notifications for completed events. The default one is FALSE
#'@return A response
#'@examples
#'\dontrun{
#'# upload to test erver
#'upload(x)
#'
#'# upload to production
#'upload(x, live = T)
#'
#'# upload from a list
#'   upload_sheets <- function(x, live = F) {
#' x %>%
#'  map(., function(y) upload(y, live))
#' }
#'
#' upload_sheets(x)
#'
#'}
upload <- function(x, live = FALSE,
                   importStrategy = "CREATE",
                   dryRun = F,
                   skipNotifications = F){

  path = "api/events"
  query = paste0("importStrategy=",importStrategy)

  if (live){
    url <- httr::modify_url("https://data.psi-mis.org", path = path, query = query)
  }

  if (!live){
    url <- httr::modify_url("https://staging.psi-mis.org", path = path, query = query)
  }

  resp <- httr::POST(url,
                     body = toJSON(list(events = x), auto_unbox = T),
                     content_type_json())


  if (http_type(resp) != "application/json"){
    stop("PSI - MIS API did not return json", call. = F)
  }

  parsed <- fromJSON(content(resp,"text"))


  if (http_error(resp)){
    warning(
      sprintf("PSI - MIS API request failed [%s]\n%s\n<%s>",
              status_code(resp),
              error_description(parsed),
              "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"),
      call. = F
    )

  }

  #parsed

  structure(
    list(content = parsed,
         message = parsed$message,
         response = resp),
    class = "upload")

}


print.upload <- function(x,...){
  cat("[PSI - MIS ", x$message, "]\n", sep = "")
  str(x$content)
  invisible(x)
}


error_description <- function(x){
  x$response$importSummaries$description %>%
    unique(.) %>%
    .[!is.na(.)] %>%
    paste0(., collapse = "")
}



#' #' Delete events
#' #'
#' delete_event <- function(program_id, startDate, live = F){
#'
#'   ua <- user_agent("https://github.com/INyabuto/a360importer")
#'
#'   path <- "api/events"
#'   query <- paste0("program=",program_id,"&lastUpdatedStartDate=", startDate,"&paging=false")
#'
#'   if (live){
#'     url <- modify_url("https://data.psi-mis.org/", path = path, query = query)
#'     url2 <- modify_url("https://data.psi-mis.org/", path = path)
#'   }
#'
#'
#'   if (!live){
#'     url <- modify_url("https://staging.psi-mis.org/", path = path, query = query)
#'     url2 <- modify_url("https://staging.psi-mis.org/", path = path)
#'   }
#'
#'   message("=============== Downloading Events =======================")
#'
#'   resp <- httr::GET(url, ua,
#'                     httr::timeout(300))
#'
#'   #resp <- httr::GET("https://data.psi-mis.org/api/events?program=b7dXdo2F35y&lastUpdatedStartDate=2019-12-11&paging=false")
#'
#'   events <- fromJSON(content(resp,"text"))$events
#'
#'   if (!is.null(events)){
#'
#'     message("================ Deleting Events ============================")
#'
#'     pb <- txtProgressBar(max = length(events$event), style = 2)
#'     for (i in seq_along(events$event)){
#'       d <- httr::DELETE(paste0(url2,"/",events$event[i]))
#'
#'       #d_parsed <- fromJSON(content(d,"text"))
#'
#'
#'       if (http_error(d)){
#'         warning(sprintf("PSI MIS API request failed [%s]\n Trying again after 5 mins",
#'                         status_code(d)))
#'         Sys.sleep(300)
#'       }
#'
#'       setTxtProgressBar(pb, i)
#'
#'     }
#'
#'     close(pb)
#'
#'
#'   }
#'
#'   d
#'
#'   # d <- lapply(events$event, function(x) httr::DELETE(paste0("https://data.psi-mis.org/api/events/",x)))
#'   # d
#' }




