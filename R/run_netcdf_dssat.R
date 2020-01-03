#' @export
run_netcdf_dssat <- function(n_dssat=3,
                             trt_start,
                             trt_end,
                             dssat_csm = '/DSSAT47/dscsm047',
                             run_mpi_dssat = paste0(dirname(dssat_csm),'/run_mpi_dssat'),
                             filex = paste0(dirname(dssat_csm),'/GridDSSAT/GridDSSAT_filex.nc'),
                             nc_out_file = 'dssat_out.nc',
                             wth_file = paste0(dirname(dssat_csm),'/Weather/idw.nc'),
                             sol_file = paste0(dirname(dssat_csm),'/Soil/statsgo_new.nc'),
                             gen_file = paste0(dirname(dssat_csm),'/Genotype/WHCER047.nc'),
                           varlist = 'HWAM',
                           other_args = NULL,
                           other_dssat_args = NULL){

  dssat_args <- c("--MPI",
                  str_c("--nc_wth=",wth_file),
                  str_c("--nc_soil=",sol_file),
                  str_c("--nc_filex=",filex),
                  str_c("--nc_gen=",gen_file),
                  other_dssat_args)
  run_mpi_call <- c("mpirun -n 1",
                    run_mpi_dssat,
                    str_c("--trt_start=",trt_start),
                    str_c("--trt_end=",trt_end),
                    str_c("--n_dssat=",n_dssat),
                    str_c("--call=",dssat_csm),
                    str_c("--varlist=",str_c(varlist, collapse = ',')),
                    str_c("--work_dir=",getwd()),
                    str_c("--nc_filex=",filex),
                    str_c("--nc_out=",nc_out_file),
                    str_c("--dssat_args=",str_c(dssat_args, collapse = ',')),
                    other_args) %>%
    str_c(collapse = ' ')

  system(run_mpi_call)

  return(invisible())
}
