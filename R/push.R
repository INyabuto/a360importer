#' @name upload
#' @title Upload a payload
#'
#' @description This function uploads events to PSI data server, `data.psi-mis.org`.
#'
#' @details
#' \code{upload} function uploads individual events to two PSI servers; `data.psi-mis.org`
#' and `clone.psi-mis.org`. By default it sends the events to the test server available at `clone.psi-mis.org`.
#'
#'
#' @param df A data.frame object, the event payload.
#' @param importStrategy A string specifying the action to apply on the import.It provides options to Save objects of all,
#' new or update import status on the server. One of 'CREATE', 'UPDATE', 'CREATE_AND_UPDATE' and 'DELETE'.The default one is
#' "CREATE".
#' @param dryRun A logical input; Save changes on the server or just return the import summary.By default, The default one is
#' FALSE, to save the changes on the server.
#' @param skipNotifications A logical input; provides an option to send notifications for completed events. The default one is FALSE
#'
#' @importFrom httr content modify_url
#' @importFrom jsonlite fromJSON
#' @return An S3 type object with content, path and the response.
#' @export
#' @examples
#' \dontrun{
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
upload <- function(df, live = FALSE,
                   importStrategy = "CREATE",
                   dryRun = F,
                   skipNotifications = F){

  path = paste0("api/",api_version(),"/events")
  query = paste0("importStrategy=",importStrategy)

  ua <- set_agent()

  if (live){
    url <- modify_url("https://data.psi-mis.org", path = path, query = query)
  }

  if (!live){
    url <- modify_url("https://staging.psi-mis.org", path = path, query = query)
  }

  # check for internet
  check_internet()

  resp <- POST(url,ua,
               body = toJSON(list(events = df), auto_unbox = T),
               content_type_json())

  # assess the response
  check_content(resp)

  parsed <- fromJSON(content(resp,"text"))

  # check the results
  check_status_on_upload(resp, parsed)

  structure(
    list(content = parsed,
         message = parsed$message,
         response = resp),
    class = "upload")

}

#' print method for class upload
#' @noRd
print.upload <- function(x,...){
  cat("[PSI - MIS ", x$message, "]\n", sep = "")
  str(x$content)
  invisible(x)
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




