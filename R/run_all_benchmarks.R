#' @export
run_all_benchmarks <- function(benchmark_scenarios){

    run_framework <- function(framework,...){
      arg_list <- deparse(list(...),width.cutoff=500L,nlines = 1)
      if(framework == "NetCDF"){
        replacement <- "run_netcdf_dssat"
      }else if(framework == "Text"){
        replacement <- "run_text_dssat"
      }
      sys_call <- str_replace(arg_list,"^list",replacement) %>%
        str_c("R --no-restore -e 'library(GridDSSAT);",.,";mpi.quit()'")
      system(sys_call)
    }

    if(!dir.exists('results')) dir.create('results')

    time_stamp <- Sys.time() %>%
      str_replace_all('[- ]+','_')

    timings <- benchmark_scenarios %>%
      dplyr::mutate(rowid = 1:n()) %>%
      group_by(rowid) %>%
      group_modify(~mutate(.,
                           start_time = Sys.time(),
                           run = run_framework(framework,
                                               n_dssat = ncores,
                                               trt_start = trt_start,
                                               trt_end = trt_end,
                                               dssat_csm = csm,
                                               filex = filex,
                                               wth_file = wth_file,
                                               sol_file = sol_file,
                                               nc_out_file = str_replace(nc_out_file,
                                                                         'rep',
                                                                         str_c(time_stamp,'-rep'))),
                           end_time = Sys.time(),
                           exec_time = difftime(end_time,start_time,units="secs"),
                           exec_speed = 1/as.numeric(exec_time),
                           exec_speed_per_core = exec_speed/ncores))

    saveRDS(timings,str_c('results/',time_stamp,'-timings.rds'))

    return(invisible())
}

#' @export
generate_benchmarks_tbl <- function(nreps = 3,
                                 dssat_csm = '/DSSAT47/dscsm047',
                                 filex = paste0(dirname(dssat_csm),'/GridDSSAT/GridDSSAT_filex.nc'),
                                 wth_file = paste0(dirname(dssat_csm),'/Weather/idw.nc'),
                                 sol_file = paste0(dirname(dssat_csm),'/Soil/statsgo_new.nc'),
                                 varlist = 'HWAM',
                                 trt_start = 1,
                                 trt_end = 7208){

    time_stamp <- Sys.time() %>%
      str_replace_all('[- ]+','_')

    scenarios <- map(1:nreps,function(r){
        rep_out <- expand.grid(framework = c('NetCDF','Text'),
                               ncores = seq(5,20,by=5)) %>%
          dplyr::mutate(csm = str_c(dirname(dssat_csm),
                                    '/dscsm047_',
                                    if_else(framework == 'Text',
                                            'std',
                                            'mpi'))) %>%
          dplyr::mutate(trt_start = trt_start,
                        trt_end = trt_end,
                        nc_out_file = str_c('results/',
                                            'rep',r,'-',
                                            framework,'-',
                                            basename(csm),'-',
                                            ncores,'cores','.nc'),
                        varlist = varlist) %>%
          sample_frac()
        return(rep_out)
        }) %>%
      bind_rows() %>%
      ungroup() %>%
      mutate(filex = filex,
             wth_file = wth_file,
             sol_file = sol_file)

    return(scenarios)

}

