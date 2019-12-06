get_lat_lon_filex <- function(nc_flx,trno){
  field <- ncvar_get(nc_flx,'FL',start = trno, count = 1)
  lat <- ncvar_get(nc_flx,'YCRD',start = field, count = 1)
  lon <- ncvar_get(nc_flx,'XCRD',start = field, count = 1)
  lat_lon <- list(lat=lat,lon=lon)
  return(lat_lon)
}
