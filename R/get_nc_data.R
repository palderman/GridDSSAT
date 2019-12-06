#' @export
get_tunnel_distance_coordinates <- function(lat,lon){
  rad_fac <- pi/180
  tdist <- data.frame(dim1 = cos(lat*rad_fac)*cos(lon*rad_fac),
                      dim2 = cos(lat*rad_fac)*sin(lon*rad_fac),
                      dim3 = sin(lat*rad_fac))
  return(tdist)
}

#' @export
construct_lat_lon_all <- function(nc){

  latitude <- ncvar_get(nc,'latitude')
  longitude <- ncvar_get(nc,'longitude')

  lat_lon_all <- expand.grid(lat_ind=1:length(latitude),
                             lon_ind=1:length(longitude))

  lat_lon_all$latitude <- latitude[lat_lon_all$lat_ind]
  lat_lon_all$longitude <- longitude[lat_lon_all$lon_ind]

  tdist <- get_tunnel_distance_coordinates(lat_lon_all$latitude,
                                           lat_lon_all$longitude)

  lat_lon_all <- cbind(lat_lon_all,tdist)

  return(lat_lon_all)
}

#' @export
get_lat_lon_i <- function(lat_lon_all,lat_pt,lon_pt){

  pt_tdist <- get_tunnel_distance_coordinates(lat_pt,lon_pt)

  ind <- which.min((lat_lon_all$dim1 - pt_tdist$dim1)^2+
                   (lat_lon_all$dim2 - pt_tdist$dim2)^2+
                   (lat_lon_all$dim3 - pt_tdist$dim3)^2)

  lat_i <- lat_lon_all$lat_ind[ind]
  lon_i <- lat_lon_all$lon_ind[ind]

  lat_lon_i <- list(lat_i=lat_i,lon_i=lon_i)

  return(lat_lon_i)
}

#' @export
open_nc_input <- function(nc_file_name){

  nc <- nc_open(nc_file_name)

  lat_lon_index <- construct_lat_lon_all(nc)

  nc_input <- list(nc = nc,
                   lat_lon_index = lat_lon_index)

  return(nc_input)
}
