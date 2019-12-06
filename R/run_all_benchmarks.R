run_all_benchmarks <- function(trt_start,trt_end,
                            filex = '/home/palderman/dssat47/Baird/ASA.nc',
                            nc_out_file = 'dssat_out.nc',
                            dssat_csm = '/DSSAT47/dscsm047',
                            wth_file = '/home/palderman/dssat47/Weather/idw.nc',
                            sol_file = '/home/palderman/dssat47/Soil/statsgo_new.nc',
                            varlist = 'HWAM'){
    benchmark_scenarios <- expand.grid(framework = c('NetCDF','Text'),
                                     ncores = seq(5,20,by=5)) %>%
    dplyr::sample_frac()

}

