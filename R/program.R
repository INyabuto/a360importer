#' GET NG RH A360 - A2. 9ja Girls & MMA Events program data elements
#'
#' \code{prog_des_9ja} Retrieves all the data elements from NG RH A360 - A2. 9ja Girls & MMA Events program.
#'
#' @return An S3 object

prog_des_9ja <- function(){

  path = "api/29/programStages/BCnz7XE4olS"
  query = "fields=programStageDataElements[dataElement[id,name]]"

  ua <- user_agent("https://github.com/INyabuto/a360importer")

  url <- modify_url("https://data.psi-mis.org", path = path, query = query)

  resp <- GET(url, ua)

  if (http_type(resp) != "application/json"){
    stop("PSI - MIS API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"))

  # Turn Http errors into R

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


  structure(
    list(
      content = parsed$programStageDataElements$dataElement,
      path = path,
      response = resp),
    class = "prog_des_9ja")

}


print.prog_des_9ja <- function(x,...){
  cat("[PSI - MIS ", x$path, "]\n", sep = "")
  str(x$content)
  invisible(x)
}


#'GET NG RH A360 - A3. Provider Client Record program data elements
#'
#'\code{prog_des_prov} Retrieves all the data elements from NG RH A360 - A3. Provider Client Record program
#'
#'@return An S3 object
prog_des_prov <- function(){
  path <- "api/29/programStages/ainkgKtBFFQ"
  query <-  "fields=programStageDataElements[dataElement[id,name]]"

  ua <- user_agent("https://github.com/INyabuto/a360importer")

  url = modify_url("https://data.psi-mis.org", path = path, query = query)

  resp <- GET(url, ua)

  if (http_type(resp) != "application/json"){
    stop("PSI MIS API did not return a json", call. = FALSE)
  }

  parsed <- fromJSON(content(resp,"text"))

  # Turn HTTP errors into R

  if (http_error(resp)){
    stop(sprintf("PSI - MIS API request failed [%s]\n%s\n<%s>",
                 status_code(resp),
                 parsed$message,
                 "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"),
         call. = FALSE)
  }

  structure(
    list(
      content = parsed$programStageDataElements$dataElement,
      path = path,
      response = resp
    ),
    class = "prog_des_prov"
  )
}


print.prog_des_prov <- function(x,...){
  cat("[PSI - MIS ", x$path, "]\n", sep = "")
  str(x$content)
  invisible(x)
}

