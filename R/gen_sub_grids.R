#' Create sub-grids of varying spatial scale
#'
#' @param file_name the name of the file from which to extract the sub-grid
#' @param ref_lat the latitude in decimal degrees for the lower left point of the sub-grid
#' @param ref_lon the longitude in decimal degrees for the lower left point of the sub-grid
#' @param no_pts the number of points in one side of the sub-grid (no_pts x no_pts square)
#'
#' @export
#'
sub_grid <- function(file_name,ref_lat,ref_lon,no_pts){

  # Extract file extension from original input file name
  file_ext <- file_name %>%
    str_extract('\\.nc.*')

  # Construct file name for sub-grid
  new_file_name <- file_name %>%
    str_remove(file_ext) %>%
    str_c('_',no_pts,'x',no_pts,file_ext)

  # Open original file and extract latitude and longitude dimensions
  nc <- nc_open(file_name)
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  nc_close(nc)

  # Find latitude and longitude indices that correspond to ref_lat and ref_lon
  lat_i <- which.min(abs(lat - ref_lat)) %>%
    {c(.,.-no_pts+1)}
  lon_i <- which.min(abs(lon - ref_lon)) %>%
    {c(.,.+no_pts-1)}

  # Construct call to ncks for extracting sub-grid
  ncks_call <- 'ncks -F -O' %>% # -F to match R indexing
    str_c(' -d latitude,',lat_i[1],',',lat_i[2]) %>% # Add latitude hyperslab
    str_c(' -d longitude,',lon_i[1],',',lon_i[2]) %>% # Add longitude hyperslab
    str_c(' ',file_name) %>% # Add original file name
    str_c(' ',new_file_name) # Add new file name

  # Run ncks call
  system(ncks_call)

  return(invisible())
}
#' Create sub-grids of filex for varying spatial scales
#'
#' @param file_name the name of the file from which to extract the sub-grid
#' @param ref_lat the latitude in decimal degrees for the lower left point of the sub-grid
#' @param ref_lon the longitude in decimal degrees for the lower left point of the sub-grid
#' @param no_pts the number of points in one side of the sub-grid (no_pts x no_pts square)
#'
#' @export
#'
sub_grid_filex <- function(file_name,ref_lat,ref_lon,no_pts){

  # Extract file extension from original input file name
  file_ext <- file_name %>%
    str_extract('\\.nc.*')

  # Construct file name for sub-grid
  new_file_name <- file_name %>%
    str_remove(file_ext) %>%
    str_c('_',no_pts,'x',no_pts,file_ext)

  # Open original file and extract latitude and longitude dimensions
  nc <- nc_open(file_name)
  lat <- ncvar_get(nc,'YCRD') %>%
    unique() %>%
    sort()
  lon <- ncvar_get(nc,'XCRD') %>%
    unique() %>%
    sort()
  nc_close(nc)

  # Find latitude and longitude indices that correspond to ref_lat and ref_lon
  lat_i <- which.min(abs(lat - ref_lat)) %>%
    {c(.,.+no_pts-1)}
  lon_i <- which.min(abs(lon - ref_lon)) %>%
    {c(.,.+no_pts-1)}

  new_coords <- expand.grid(YCRD = lat[lat_i[1]:lat_i[2]],
                            XCRD = lon[lon_i[1]:lon_i[2]])

  # Construct call to ncks for extracting sub-grid
  ncks_call <- 'ncks -F -O' %>% # -F to match R indexing
    str_c(' -d FIELDS,1,',no_pts^2) %>% # Add FIELDS hyperslab
    str_c(' -d TREATMENTS,1,',no_pts^2) %>% # Add TREATMENTS hyperslab
    str_c(' -d SIMULATION\\ CONTROLS,1,1') %>% # Add SIMULATION CONTROLS hyperslab
    str_c(' -d PLANTING\\ DETAILS,1,1') %>% # Add PLANTING DETAILS hyperslab
    str_c(' -d FERTILIZERS,1,1') %>% # Add FERTILIZERS hyperslab
    str_c(' ',file_name) %>% # Add original file name
    str_c(' ',new_file_name) # Add new file name

  # Run ncks call
  system(ncks_call)

  # Adjust variables for new sub-grid
  nc <- nc_open(new_file_name,write=TRUE)
    ncvar_put(nc,'XCRD',new_coords$XCRD)
    ncvar_put(nc,'YCRD',new_coords$YCRD)
    ncvar_put(nc,'MF',rep(1,no_pts^2))
    ncvar_put(nc,'MP',rep(1,no_pts^2))
    ncvar_put(nc,'SM',rep(1,no_pts^2))
  nc_close(nc)

  return(invisible())
}

#' Generate sub-grids for 1x1, 5x5, 10x10, 50x50 and Oklahoma spatial scales
#'
#' @param file_name the name of the file from which to extract the sub-grid
#' @param ref_lat the latitude in decimal degrees for the lower left point of the sub-grid
#' @param ref_lon the longitude in decimal degrees for the lower left point of the sub-grid
#' @param no_pts the number of points in one side of the sub-grid (no_pts x no_pts square)
#'
#' @export
#'
gen_all_sub_grids <- function(dssat_dir='/carl-data/shared/carl/DSSAT47',
                              file_path = c('/Weather/idw.nc',
                                          '/Soil/statsgo.nc'),
                              filex_path = c('/gridded_DSSAT/grid_filex.nc'),
                              ref_lat = 34.5,
                              ref_lon = -99.35,
                              grid_size=c(1,5,10,25,50)){

  files <- dssat_dir %>%
    str_c(file_path)

  filex <- dssat_dir %>%
    str_c(filex_path)

  for(grid in grid_size){
    for(file in files){
      sub_grid(file_name = file,
               ref_lat = ref_lat,
               ref_lon = ref_lon,
               no_pts = grid)
    }
    sub_grid_filex(file_name = filex,
                   ref_lat = ref_lat,
                   ref_lon = ref_lon,
                   no_pts = grid)
  }

  return(invisible())
}
