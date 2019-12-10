#'Generate payload
#'
#'@param dt A list of tranformed df
#'@return list payload
generate_payload <- function(dt, program_id = "program_id", status = "COMPLETED", storedBy = "old / archive import"){

  dt2 <- dt %>% dplyr::mutate(program = rep(program_id, n()),
                       status = rep(status, n()),
                       storedBy = rep(storedBy, n()),
                       odt5NRDyZEM = map_chr(.$odt5NRDyZEM, as.character)) %>%
    dplyr::select(program,orgUnit,eventDate,status,storedBy, everything())

  #dt2$dataValues <- dt2 %>% map(., gather_cols)

  #map(dt2, gather_cols)

  dt2 %>%  split(.,(as.numeric(row.names(.)) - 1) %/% 1)




}

#'Map payload
#'
#'Apply payload to a list of data.frames or data.tables
#'
#'@param x A list of data.frame or data.tables
#'@return A mapped lsit of data.frames
map_payload <- function(x){
  x %>%
  map(.,format_payload)

}

#'Format payload
#'
#'generate an event payload from a dataframe object.
#'
#'@param dt A data.table or data.frame
#'@return Event payload
format_payload <- function(dt){
  # dt %>%
  #   split(.,(as.numeric(row.names(.)) - 1) %/% 1)

  dt$dataValues <- list(dt %>% tidyr::pivot_longer(cols = 6:length(.),
                             names_to = "dataElement",
                             values_to = "value",
                             values_ptypes = list(val = 'character')) %>%
    dplyr::select(dataElement,value))

  dt %>%
    dplyr::select(program,orgUnit,eventDate,status,storedBy,dataValues)



}


#r <- GET("https://data.psi-mis.org/api/events?program=etj54nrbhLx")
