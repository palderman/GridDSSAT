#' @export
run_text_dssat <- function(n_dssat=1,
                           trt_start,
                           trt_end,
                           dssat_csm = '/DSSAT47/dscsm047',
                           filex = paste0(dirname(dssat_csm),'/GridDSSAT/GridDSSAT_filex.nc'),
                           nc_out_file = 'dssat_out.nc',
                           wth_file = paste0(dirname(dssat_csm),'/Weather/idw.nc'),
                           sol_file = paste0(dirname(dssat_csm),'/Soil/statsgo_new.nc'),
                           varlist = 'HWAM'){

  # In case R exits unexpectedly, have it automatically clean up
  # resources taken up by Rmpi (slaves, memory, etc...)
  assign('.Last',function(){
    if (is.loaded("mpi_initialize")){
      if (mpi.comm.size(1) > 0){
        mpi.close.Rslaves()
      }
      .Call("mpi_finalize")
    }
  },
  envir = .GlobalEnv)

  total_trts = trt_end - trt_start + 1

#  max_cores <- Rmpi::mpi.universe.size()

  if(n_dssat > total_trts) n_dssat <- total_trts

#  if(n_dssat > max_cores) n_dssat <- max_cores

  trtno <- trt_start:trt_end

  spawn_dssat_children(n_children = n_dssat,
                       trtno = trtno,
                       varlist = varlist,
                       dssat_csm = dssat_csm,
                       wth_file = wth_file,
                       sol_file = sol_file,
                       filex = filex)

  # Read coordinate and nyears information from file X
  nc_flx <- nc_open(filex)

  crop_code <- ncvar_get(nc_flx, "CR")

  xcrd <- ncvar_get(nc_flx, 'XCRD')
  ycrd <- ncvar_get(nc_flx, 'YCRD')

  lon <- unique(xcrd) %>% sort()
  lat <- unique(ycrd) %>% sort()

  xcrd_i <- factor(xcrd,levels = lon) %>% as.integer()
  ycrd_i <- factor(ycrd,levels = lat) %>% as.integer()

  sim <- ncvar_get(nc_flx, 'SM', start = trt_start, count = 1)

  nyears <- ncvar_get(nc_flx, 'NYERS', start = sim, count = 1)

  nc_close(nc_flx)

  # Define dimensions and corresponding variables for output file
  lat_dim <- ncdim_def("lat", units = "", vals = 1:length(lat), create_dimvar = FALSE)
  lon_dim <- ncdim_def("lon", units = "", vals = 1:length(lon), create_dimvar = FALSE)
  season_dim <- ncdim_def("season", units = "", vals = as.integer(1:nyears), create_dimvar = FALSE)

  lon_var <- ncvar_def("lon", dim = lon_dim, units = "", prec = "float", missval = -99)
  lat_var <- ncvar_def("lat", dim = lat_dim, units = "", prec = "float", missval = -99)
  season_var <- ncvar_def("season", dim = season_dim, units = "", prec = "integer", missval = -99)

  all_dims <- list(lon_dim,lat_dim,season_dim)

  # Define all variable in varlist
  all_vars <- lapply(varlist,function(v){
      ncvar_def(v,dim = all_dims, units = "", prec = "float", missval = -99)
    }) %>%
    {c(list(lon_var,lat_var,season_var),.)}

  # Create output file
  out_file <- nc_create(nc_out_file,vars = all_vars)

  # Load latitude, longitude, and season number into output file
  ncvar_put(out_file,'lat',lat)
  ncvar_put(out_file,'lon',lon)
  ncvar_put(out_file,'season',as.integer(1:nyears))

  seasonal <- mpi.remote.exec({run_all_treatments(wth_input,
                                                  sol_input,
                                                  filex_input,
                                                  varlist,
                                                  trtno)})

  write_regsitry(nc_out = out_file,
                 trtno = trtno,
                 dim_size = nyears,
                 xcrd_i = xcrd_i,
                 ycrd_i = ycrd_i,
                 seasonal = seasonal)

  nc_close(out_file)

  mpi.bcast.cmd({
    setwd('..')
    unlink(paste0(work_dir,'/dssat_',mpi.comm.rank()),recursive = TRUE)
    })
  
  # Tell all slaves to close down, and exit the program
  mpi.close.Rslaves(dellog = TRUE)
  
  

  return(invisible())

}
