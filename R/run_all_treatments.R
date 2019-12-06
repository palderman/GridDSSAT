#' @export
run_all_treatments <- function(wth_input,sol_input,filex_input,
                               varlist,trtno){

  seasonal <- trtno %>%
    map(run_grid_point,
        wth_input = wth_input,
        sol_input = sol_input,
        filex_input = filex_input,
        varlist = varlist)

  return(seasonal)
}
