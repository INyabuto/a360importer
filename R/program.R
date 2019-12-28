#' @name prog_des_9ja
#' @title Program data elements (9ja)
#'
#' @description Retrieve all the data elements from `NG RH A360 - A2. 9ja Girls & MMA Events` program.
#'
#' @details \code{prog_des_9ja} retrieves all the 9ja Girls and MMA Events data elements from the PSI data server
#'     (https://data.psi-mis.org). It selects the data element name and id from the program stage (id = BCnz7XE4olS)
#'      and returns an S3 type object with the content, path, and the response.
#'
#' @importFrom httr content GET modify_url
#' @importFrom jsonlite fromJSON
#'
#' @export
#' @return An S3 object with information about the content, path and the reponse
#' @examples
#' \dontrun{
#' prog_des_9ja()
#' }

prog_des_9ja <- function(){

  path = paste0("api/",api_version(),"/programStages/BCnz7XE4olS")
  query = "fields=programStageDataElements[dataElement[id,name]]"

  ua <- set_agent()

  url <- modify_url("https://data.psi-mis.org", path = path, query = query)

  # check for internet
  check_internet()

  resp <- GET(url, ua)

  # assess the response
  check_content(resp)

  parsed <- fromJSON(content(resp, "text"))

  # check the result
  check_status(resp, parsed)

  structure(
    list(
      content = parsed$programStageDataElements$dataElement,
      path = path,
      response = resp),
    class = "prog_des_9ja")

}

#' print method for the class program_des_9ja
print.prog_des_9ja <- function(x,...){
  cat("[PSI - MIS ", x$path, "]\n", sep = "")
  str(x$content)
  invisible(x)
}



#' @name assign_ous
#' @title Assign OUS to a program
#'
#' @description Updates the organisation units of a program.
#'
#' @param df A data.frame with organisationUnit ids
#' @param program_id The id of the Program to update.
#' @param live A logical input (TRUE/FALSE) specifying the change
#'       or where to apply to the changes. The default option is FALSE;
#'       the changes will be made at the test server. You can specify this option
#'        to TRUE to apply the changes in production.
#'
#' @importFrom httr content GET POST timeout
#' @importFrom jsonlite fromJSON
#' @importFrom plyr rbind.fill
#'
#' @export
#' @return A response
#' @details \code{assign_ous} updates the existing program ous with new organisation units (ous).
#' @examples
#' \dontrun{
#' assign_ous(df, program_id = "BCnz7XE4olS")
#' assign_ous(df, program_id = "BCnz7XE4olS", live = TRUE)
#' }
assign_ous <- function(df, program_id = "program_id", live = F){

  path <- paste0("api/",api_version(),"/programs/",program_id)
  path2 <- paste0("api/",api_version(),"metadata")
  query <- "importStrategy=UPDATE"

  ua <- set_agent()

  if (live) {
    url <- modify_url("https://data.psi-mis.org/", path = path)
    url2 <- modify_url("https://data.psi-mis.org/", path = path2, query = query)
  }

  if (!live) {
    url <- modify_url("https://clone.psi-mis.org/", path = path)
    url2 <- modify_url("https://clone.psi-mis.org/", path = path2, query = query)
  }

  # check for internet
  check_internet()

  resp <- httr::GET(url, ua,
                    httr::timeout(60))

  # check the response
  check_content(resp)

  parsed <- fromJSON(content(resp,"text"))


  parsed$organisationUnits <- plyr::rbind.fill(parsed$organisationUnits,df["id"])

  d <- POST(url2,
            body = toJSON(list(programs = list(parsed)), auto_unbox = T),
            content_type_json())

  d



}


#' @name prog_des_prov
#' @title Program data elements (provider)
#'
#' @description Retrieve all the data elements from `NG RH A360 - A3. Provider Client Record` program.
#'
#' @details \code{prog_des_prov} retrieves all the NG RH A360 - A3. Provider Client Records data elements from the PSI data server
#'     (https://data.psi-mis.org). It selects the data element name and id from the program stage (id = ainkgKtBFFQ)
#'      and returns an S3 type object with the content, path, and the response.
#'
#' @importFrom httr content  GET  modify_url
#' @importFrom jsonlite fromJSON
#'
#' @export
#' @return An S3 object with information about the content, path and the reponse
#' @examples
#' \dontrun{
#' prog_des_prov()
#' }
prog_des_prov <- function(){
  path <- paste0("api/",api_version(),"/programStages/ainkgKtBFFQ")
  query <-  "fields=programStageDataElements[dataElement[id,name]]"

  # set user agennt
  ua <- set_agent()

  url = modify_url("https://data.psi-mis.org", path = path, query = query)

  # check for internet
  check_internet()

  resp <- GET(url, ua)

  # asses the response
  check_content(resp)

  parsed <- fromJSON(content(resp,"text"))

  # check  the results
  check_status(resp, parsed)

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

