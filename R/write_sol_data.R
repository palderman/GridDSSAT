#' @export
write_sol_data <- function(nc_sol,lat,lon,lat_i,lon_i){

  dim_order <- names(nc_sol$dim)[nc_sol$var$SLB$dimids+1]

  start <- c(latitude=lat_i,longitude=lon_i,layer=1)
  start <- start[dim_order]

  count <- c(latitude=1,longitude=1,layer=-1)
  count <- count[dim_order]

  lyr_dim <- which(dim_order == "layer")

  PEDON <- 'GRID000001'
  SOURCE <- as.character(NA)
  TEXTURE <- as.character(NA)
  DEPTH <- as.numeric(NA)
  DESCRIPTION <- as.character(NA)
  SITE <- as.character(NA)
  COUNTRY <- as.character(NA)
  LAT <- lat
  LONG <- lon
  `SCS FAMILY` <- as.character(NA)
  SCOM <- as.numeric(NA)
  SALB <- ncvar_get(nc_sol,"SALB",start=start[-lyr_dim],count=count[-lyr_dim])
  SLU1 <- ncvar_get(nc_sol,"SLU1",start=start[-lyr_dim],count=count[-lyr_dim])
  SLDR <- ncvar_get(nc_sol,"SLDR",start=start[-lyr_dim],count=count[-lyr_dim])
  SLRO <- ncvar_get(nc_sol,"SLRO",start=start[-lyr_dim],count=count[-lyr_dim])
  SLNF <- 1
  SLPF <- 1
  SMHB <- as.character(NA)
  SMPX <- as.character(NA)
  SMKE <- as.character(NA)
  SLB <- ncvar_get(nc_sol,"SLB",start=start,count=count)
  SLB <- SLB[!is.na(SLB)]
  count[lyr_dim] <- length(SLB)
  SLMH <- rep(as.character(NA),count[lyr_dim])
  SLLL <- ncvar_get(nc_sol,"SLLL",start=start,count=count)
  SDUL <- ncvar_get(nc_sol,"SDUL",start=start,count=count)
  SSAT <- ncvar_get(nc_sol,"SSAT",start=start,count=count)
  SRGF <- ncvar_get(nc_sol,"SRGF",start=start,count=count)
  SSKS <- ncvar_get(nc_sol,"SSKS",start=start,count=count)
  SBDM <- ncvar_get(nc_sol,"SBDM",start=start,count=count)
  SLOC <- ncvar_get(nc_sol,"SLOC",start=start,count=count)
  SLCL <- ncvar_get(nc_sol,"SLCL",start=start,count=count)
  SLSI <- ncvar_get(nc_sol,"SLSI",start=start,count=count)
  SLCF <- ncvar_get(nc_sol,"SLCF",start=start,count=count)
  SLNI <- rep(as.numeric(NA),count[lyr_dim])
  SLHW <- rep(as.numeric(NA),count[lyr_dim])
  SLHB <- rep(as.numeric(NA),count[lyr_dim])
  SCEC <- rep(as.numeric(NA),count[lyr_dim])
  SADC <- rep(as.numeric(NA),count[lyr_dim])

  v_fmt <- c(PEDON = "*%-10s", SOURCE = "  %-11s", TEXTURE = " %-5s", DEPTH = "%6.0f",
             DESCRIPTION = " %-s", SITE = "%-13s", COUNTRY = "%-12s", LAT = "%9.3f",
             LONG = "%9.3f", `SCS FAMILY` = " %-s", SCOM = "%6s", SALB = "%6.2f",
             SLU1 = "%6.0f", SLDR = "%6.1f", SLRO = "%6.0f", SLNF = "%6.0f",
             SLPF = "%6.0f", SMHB = "%6s", SMPX = "%6s", SMKE = "%6s", SLB = "%6.0f",
             SLMH = "%6s", SLLL = "%6.3f", SDUL = "%6.3f", SSAT = "%6.3f",
             SRGF = "%6.3f", SSKS = "%6.3f", SBDM = "%6.2f", SLOC = "%6.2f",
             SLCL = "%6.0f", SLSI = "%6.0f", SLCF = "%6.0f", SLNI = "%6.2f",
             SLHW = "%6.1f", SLHB = "%6f", SCEC = "%6f", SADC = "%6f")


  tier_info <- list(c("PEDON", "SOURCE", "TEXTURE", "DEPTH", "DESCRIPTION"),
                    c("SITE", "COUNTRY", "LAT", "LONG", "SCS FAMILY"),
                    c("SCOM", "SALB", "SLU1", "SLDR", "SLRO", "SLNF", "SLPF",
                      "SMHB", "SMPX", "SMKE"),
                    c("SLB", "SLMH", "SLLL", "SDUL", "SSAT", "SRGF", "SSKS",
                      "SBDM", "SLOC", "SLCL", "SLSI", "SLCF", "SLNI", "SLHW",
                      "SLHB", "SCEC", "SADC"))

  sol_data <- tibble(PEDON = PEDON,
                     SOURCE = SOURCE,
                     TEXTURE = TEXTURE,
                     DEPTH = DEPTH,
                     DESCRIPTION = DESCRIPTION,
                     SITE = SITE,
                     COUNTRY = COUNTRY,
                     LAT = LAT,
                     LONG = LONG,
                     `SCS FAMILY` = `SCS FAMILY`,
                     SCOM = SCOM,
                     SALB = SALB,
                     SLU1 = SLU1,
                     SLDR = SLDR,
                     SLRO = SLRO,
                     SLNF = SLNF,
                     SLPF = SLPF,
                     SMHB = SMHB,
                     SMPX = SMPX,
                     SMKE = SMKE,
                     SLB = list(SLB),
                     SLMH = list(SLMH),
                     SLLL = list(SLLL),
                     SDUL = list(SDUL),
                     SSAT = list(SSAT),
                     SRGF = list(SRGF),
                     SSKS = list(SSKS),
                     SBDM = list(SBDM),
                     SLOC = list(SLOC),
                     SLCL = list(SLCL),
                     SLSI = list(SLSI),
                     SLCF = list(SLCF),
                     SLNI = list(SLNI),
                     SLHW = list(SLHW),
                     SLHB = list(SLHB),
                     SCEC = list(SCEC),
                     SADC = list(SADC)) %>%
    DSSAT::as_DSSAT_tbl(v_fmt = v_fmt,
                        tier_info = tier_info)

  DSSAT::write_sol(sol_data,"SOIL.SOL",append = FALSE)

  return(invisible())
}
