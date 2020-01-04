#' @export
write_wth_data <- function(nc_wth,lat,lon,lat_i,lon_i){

  dim_order <- names(nc_wth$dim)[nc_wth$var$RAIN$dimids+1]

  start <- c(latitude=lat_i,longitude=lon_i,DATE=1)
  start <- start[dim_order]

  count <- c(latitude=1,longitude=1,DATE=-1)
  count <- count[dim_order]

  date_dim <- which(dim_order == "DATE")

  TAMP <- ncvar_get(nc_wth,'TAMP',start=start[-date_dim],count=count[-date_dim])
  TAV <- ncvar_get(nc_wth,'TAV',start=start[-date_dim],count=count[-date_dim])
  ELEV <- ncvar_get(nc_wth,'ELEV',start=start[-date_dim],count=count[-date_dim])
  REFHT <- ncvar_get(nc_wth,'REFHT',start=start[-date_dim],count=count[-date_dim])
  WNDHT <- ncvar_get(nc_wth,'WINDHT',start=start[-date_dim],count=count[-date_dim])

  DATE <- ncvar_get(nc_wth,'DATE')
  SRAD <- ncvar_get(nc_wth,'SRAD',start=start,count=count)
  TMAX <- ncvar_get(nc_wth,'TMAX',start=start,count=count)
  TMIN <- ncvar_get(nc_wth,'TMIN',start=start,count=count)
  RAIN <- ncvar_get(nc_wth,'RAIN',start=start,count=count)
  WIND <- ncvar_get(nc_wth,'WIND',start=start,count=count)
  RHUM <- ncvar_get(nc_wth,'RHUM',start=start,count=count)

  GENERAL <- tibble(INSI = "GRID",
                    LAT = lat,
                    LONG = lon,
                    ELEV = ELEV,
                    TAV = TAV,
                    AMP = TAMP,
                    REFHT = REFHT,
                    WNDHT = WNDHT) %>%
    DSSAT::as_DSSAT_tbl(v_fmt = c(INSI = "%6s",
                                  LAT = "%9.3f",
                                  LONG = "%9.3f",
                                  ELEV = "%6.0f",
                                  TAV = "%6.1f",
                                  AMP = "%6.1f",
                                  REFHT = "%6.1f",
                                  WNDHT = "%6.1f"))

  daily_wth <- tibble(DATE = {as.character(DATE) %>%
      as.POSIXct(format="%Y%j",tz="UTC")},
      SRAD = SRAD,
      TMAX = TMAX,
      TMIN = TMIN,
      RAIN = RAIN,
      WIND = WIND,
      RHUM = RHUM) %>%
    DSSAT::as_DSSAT_tbl(v_fmt = c(DATE = "%5s",
                                  SRAD = "%6.1f",
                                  TMAX = "%6.1f",
                                  TMIN = "%6.1f",
                                  RAIN = "%6.1f",
                                  WIND = "%6.0f",
                                  RHUM = "%6.1f"))

  attr(daily_wth,"GENERAL") <- GENERAL

  DSSAT::write_wth(daily_wth,"GRID0001.WTH")

  return(invisible())
}
