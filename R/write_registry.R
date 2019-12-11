#' @export
write_regsitry <- function(nc_out,trtno,dim_size,xcrd_i,ycrd_i,seasonal){

  curr_trt = 1

  for(i in 1:length(seasonal)){ # index for DSSAT instances
    for(j in 1:length(seasonal[[i]])){ # index for treatments within DSSAT instance
      var_names <- names(seasonal[[i]][[j]])
      for(k in 1:length(var_names)){ # index for variables within treatment
        ncvar_put(nc_out,
                  var_names[k],
                  start = c(xcrd_i[trtno[curr_trt]],ycrd_i[trtno[curr_trt]],1),
                  count = c(1,1,dim_size),
                  vals = seasonal[[i]][[j]][[k]])
      }
      curr_trt <- curr_trt + 1
    }
  }

}
