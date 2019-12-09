#'Map Clinics

#'map clinics in the files with exising one in DHIS2.
#'
#'@param dt A vector of character or string to modify
#'@return uid of the mapped clinic
map_clinics <- function(dt,ng_ous){

  dt %>% dplyr::mutate(Facility = plyr::mapvalues(.$Facility,
                                                  from = ng_ous$Facility,
                                                  to = ng_ous$id), warn_missing = F)
}








