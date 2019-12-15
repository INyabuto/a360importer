#'Generate payload
#'
#'This function turns a mapped data frame or data.table to a payload ready for upload.
#'
#'\code{generate_payload} is typically used in a map or apply function but you can also directly supply a mapped data.table or
#'data frame.
#'
#'@param dt A mapped data.table or data frame to transform.
#'@param program_id The ID of the event program.
#'@param status The status of events. By default, events are marked as completed.
#'@param storedBy A reference of the imports. If its an historical upload it better to lebale them as "old / archive import".
#'@return A transformed data.frame.
#'@examples
#'\dontrun{
#'# transform a single dt to a payload
#'generate_payload(dt, program_id = "idgtsybkji2")
#'
#'# transform a list of data frames to a payload
#'purrr::map(dt, generate_payload)
#'
#'}
generate_payload <- function(dt, program_id = "program_id", status = "COMPLETED", storedBy = "old / archive import"){

  dt2 <- dt %>% dplyr::mutate(program = rep(program_id, n()),
                       status = rep(status, n()),
                       storedBy = rep(storedBy, n()),
                       event = map_chr(rep(11, n()), generateUID),
                       odt5NRDyZEM = map_chr(.$odt5NRDyZEM, as.character)) %>%
    dplyr::select(program,orgUnit,event,eventDate,status,storedBy,everything())

  #dt2$dataValues <- dt2 %>% map(., gather_cols)

  #map(dt2, gather_cols)

  #dt2 %>%  split(.,(as.numeric(row.names(.)) - 1) %/% 1)

  dt2$dataValues <- map(dt2 %>%
                         split(.,(as.numeric(row.names(.)) - 1) %/% 1), function(x)
                           tidyr::pivot_longer(x, cols = 7:length(x),
                                               names_to = "dataElement",
                                               values_to = "value",
                                               values_ptypes = list(val = 'character')) %>%
                         dplyr::select(dataElement,value))


  dt2 %>%
    dplyr::select(program,orgUnit,event,eventDate,status,storedBy,dataValues)


}
