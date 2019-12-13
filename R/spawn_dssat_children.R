#' @export
spawn_dssat_children <- function(n_children,trtno,varlist,
                                 dssat_csm,wth_file,sol_file,filex){

  mpi.spawn.Rslaves(nslaves=n_children)

  work_dir <- getwd()

  mpi.bcast.Robj2slave(work_dir)
  mpi.bcast.Robj2slave(wth_file)
  mpi.bcast.Robj2slave(sol_file)
  mpi.bcast.Robj2slave(filex)
  mpi.bcast.Robj2slave(dssat_csm)
  mpi.bcast.Robj2slave(varlist)

  mpi.bcast.cmd({
    library(DSSAT)
    options(DSSAT.CSM = dssat_csm)
    library(GridDSSAT)
    my_work_dir <- paste0(work_dir,'/dssat_',mpi.comm.rank())
    dir.create(my_work_dir)
    setwd(my_work_dir)
    wth_input <- open_nc_input(wth_file)
    sol_input <- open_nc_input(sol_file)
    filex_input <- list(nc=nc_open(filex),CR='WH')
  })

  # Send arguments to child process:
  total_trts <- length(trtno)
  trt_ind <- 1
  for(i in 1:n_children){
    ntrt <- total_trts %/% n_children
    if(i <= total_trts %% n_children) ntrt <- ntrt + 1
    trts <- trtno[trt_ind:(trt_ind+ntrt-1)]
    mpi.send.Robj(trts,dest=i,tag=0)
    trt_ind <- trt_ind + ntrt
  }
  mpi.bcast.cmd({trtno <- mpi.recv.Robj(0,0)})

}
