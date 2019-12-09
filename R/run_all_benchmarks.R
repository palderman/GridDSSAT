#' @export
run_all_benchmarks <- function(dssat_csm = '/DSSAT47/dscsm047',
                               filex = paste0(dirname(dssat_csm),'/GridDSSAT/GridDSSAT_filex.nc'),
                               wth_file = paste0(dirname(dssat_csm),'/Weather/idw.nc'),
                               sol_file = paste0(dirname(dssat_csm),'/Soil/statsgo_new.nc'),
                               varlist = 'HWAM'){

    run_framework <- function(framework,...){
      if(framework == "NetCDF"){
        run_netcdf_dssat(...)
      }else if(framework == "Text"){
        run_text_dssat(...)
      }
    }

    if(!dir.exists('results')) dir.create('results')

    time_stamp <- Sys.time() %>%
      str_replace_all('[- ]+','_')

    benchmark_scenarios <- expand.grid(framework = c('NetCDF','Text'),
                                     ncores = seq(5,20,by=5),
                                     csm = str_c(dirname(dssat_csm),'/dssat_',c('std','mpi'))) %>%
      dplyr::filter( framework != "NetCDF" | str_detect(csm,'_mpi')) %>%
      dplyr::mutate(trt_start = 1,
                    trt_end = 7208,
                    nc_out_file = str_c('results/',
                                        time_stamp,'-',
                                        framework,'-',
                                        basename(dssat_csm),'-',
                                        ncores,'cores','.nc'),
                    varlist = varlist) %>%
      dplyr::sample_frac() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(timings = list(system.time({run_framework(framework,
                                                              n_dssat = ncores,
                                                              trt_start = trt_start,
                                                              trt_end = trt_end,
                                                              dssat_csm = dssat_csm,
                                                              filex = filex,
                                                              wth_file = wth_file,
                                                              sol_file = sol_file,
                                                              nc_out_file = nc_out_file)})))

    saveRDS(benchmark_scenarios,str_c('results/',time_stamp,'-timings.rds'))

    return(benchmark_scenarios)
}

