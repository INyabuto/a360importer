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
#'\dontrun{
#'upload(x)
#'}
#'
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
    stop(
      sprintf("PSI - MIS API request failed [%s]\n%s\n<%s>",
              status_code(resp),
              parsed$message,
              "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"),
      call. = F
    )
    
  }
  
  structure(
    list(content = parsed$response,
         message = parsed$message,
         response = resp),
    class("upload")
  )
  
}

print.upload <- function(x,...){
  cat("[PSI - MIS ", x$message, "]\n", sep = "")
  str(x$content)
  invisible(x)
}