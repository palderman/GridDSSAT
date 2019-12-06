#' @export
write_regsitry <- function(nc_out,trtno,dim_size,xcrd_i,ycrd_i,seasonal){

  curr_trt = rep(1,length(seasonal[[1]][[1]]))

  for(i in 1:length(seasonal)){ # index for DSSAT instances
    for(j in 1:length(seasonal[[i]])){ # index for treatments within DSSAT instance
      var_names <- names(seasonal[[i]][[j]])
      for(k in 1:(length(seasonal[[i]][[j]])/dim_size)){ # index for variables within treatment
        ncvar_put(nc_out,
                  var_names[k],
                  start = c(xcrd_i[trtno[curr_trt[k]]],ycrd_i[trtno[curr_trt[k]]],1),
                  count = c(1,1,dim_size),
                  vals = seasonal[[i]][[j]][[k]])
        curr_trt[j] <- curr_trt[j] + 1
      }
    }
  }

}
