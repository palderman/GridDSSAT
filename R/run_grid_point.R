#' @export
run_grid_point <- function(wth_input,sol_input,filex_input,varlist,trno){

  lat_lon <- get_lat_lon_filex(nc_flx = filex_input$nc,
                               trno = trno)

  write_flx_data(nc_flx = filex_input$nc,
                 CR = filex_input$CR,
                 trno = trno)

  wth_ind <- get_lat_lon_i(wth_input$lat_lon_index,
                           lat_pt = lat_lon$lat,
                           lon_pt = lat_lon$lon)

  write_wth_data(nc_wth = wth_input$nc,
                 lat = lat_lon$lat,
                 lon = lat_lon$lon,
                 lat_i = wth_ind$lat_i,
                 lon_i = wth_ind$lon_i)

  sol_ind <- get_lat_lon_i(sol_input$lat_lon_index,
                           lat_pt = lat_lon$lat,
                           lon_pt = lat_lon$lon)

  write_sol_data(nc_sol = sol_input$nc,
                 lat = lat_lon$lat,
                 lon = lat_lon$lon,
                 lat_i = sol_ind$lat_i,
                 lon_i = sol_ind$lon_i)

  DSSAT::run_dssat(run_mode = "C",
                   file_name = "GRID0001.CRX 1")

  smry <- DSSAT::read_output('Summary.OUT') %>%
    dplyr::select(one_of(varlist))

  return(smry)
}
