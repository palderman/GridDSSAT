#' @export
write_flx_data <- function(nc_flx,CR,trno){

  v_fmt_trt <- c(N = "%2.0f", R = "%2.0f", O = "%2.0f", C = "%2.0f", TNAME = " %-25s",
                 CU = "%3.0f", FL = "%3.0f", SA = "%3.0f", IC = "%3.0f", MP = "%3.0f",
                 MI = "%3.0f", MF = "%3.0f", MR = "%3.0f", MC = "%3.0f", MT = "%3.0f",
                 ME = "%3.0f", MH = "%3.0f", SM = "%3.0f")

  treatments <- tibble(N = 1,
                       R = 1,
                       O = 1,
                       C = 1,
                       TNAME = '',
                       CU = ncvar_get(nc_flx, "CU", start = trno, count = 1),
                       FL = 1,
                       SA = ncvar_get(nc_flx, "SA", start = trno, count = 1),
                       IC = ncvar_get(nc_flx, "IC", start = trno, count = 1),
                       MP = ncvar_get(nc_flx, "MP", start = trno, count = 1),
                       MI = ncvar_get(nc_flx, "MI", start = trno, count = 1),
                       MF = ncvar_get(nc_flx, "MF", start = trno, count = 1),
                       MR = ncvar_get(nc_flx, "MR", start = trno, count = 1),
                       MC = ncvar_get(nc_flx, "MC", start = trno, count = 1),
                       MT = ncvar_get(nc_flx, "MT", start = trno, count = 1),
                       ME = ncvar_get(nc_flx, "ME", start = trno, count = 1),
                       MH = ncvar_get(nc_flx, "MH", start = trno, count = 1),
                       SM = ncvar_get(nc_flx, "SM", start = trno, count = 1)) %>%
    DSSAT::as_DSSAT_tbl(v_fmt = v_fmt_trt)

  v_fmt_cul <- c(C = "%2.0f", CR = "%3s", INGENO = "%7s", CNAME = " %-s")

  cultivars <- tibble(C = treatments$CU,
                      CR = CR,
                      INGENO = ncvar_get(nc_flx, "INGENO", start = c(1,treatments$CU), count = c(-1,1)),
                      CNAME = ncvar_get(nc_flx, "CNAME", start = c(1,treatments$CU), count = c(-1,1))) %>%
    DSSAT::as_DSSAT_tbl(v_fmt = v_fmt_cul)

  v_fmt_fields <- c(L = "%2.0f", ID_FIELD = " %-9s", WSTA = "%-8s", FLSA = "%6f",
                    FLOB = "%6.0f", FLDT = "%6s", FLDD = "%6.0f", FLDS = "%6.0f",
                    FLST = "%6s", SLTX = " %-4s", SLDP = "%6.0f", ID_SOIL = "  %-11s",
                    FLNAME = "%-s", XCRD = "%16.3f", YCRD = "%16.3f", ELEV = "%10.0f",
                    AREA = "%18.0f", SLEN = "%6.0f", FLWR = "%6.0f", SLAS = "%6.0f",
                    FLHST = "%6s", FHDUR = "%6.0f")

  tier_info_fields <- list(c("L", "ID_FIELD", "WSTA", "FLSA", "FLOB", "FLDT", "FLDD",
                             "FLDS", "FLST", "SLTX", "SLDP", "ID_SOIL", "FLNAME"),
                           c("L", "XCRD", "YCRD", "ELEV", "AREA", "SLEN", "FLWR",
                             "SLAS", "FLHST", "FHDUR"))

  fields <- tibble(L = 1,
                   ID_FIELD = ncvar_get(nc_flx, "ID_FIELD", start = c(1,trno), count = c(-1,1)),
                   WSTA = 'GRID0001',
                   FLSA = ncvar_get(nc_flx, "FLSA", start = trno, count = 1),
                   FLOB = ncvar_get(nc_flx, "FLOB", start = trno, count = 1),
                   FLDT = ncvar_get(nc_flx, "FLDT", start = c(1,trno), count = c(-1,1)),
                   FLDD = ncvar_get(nc_flx, "FLDD", start = trno, count = 1),
                   FLDS = ncvar_get(nc_flx, "FLDS", start = trno, count = 1),
                   FLST = ncvar_get(nc_flx, "FLST", start = c(1,trno), count = c(-1,1)),
                   SLTX = ncvar_get(nc_flx, "SLTX", start = c(1,trno), count = c(-1,1)),
                   SLDP = ncvar_get(nc_flx, "SLDP", start = trno, count = 1),
                   ID_SOIL = 'GRID000001',
                   FLNAME = ncvar_get(nc_flx, "FLNAME", start = c(1,trno), count = c(-1,1)),
                   XCRD = ncvar_get(nc_flx, "XCRD", start = trno, count = 1),
                   YCRD = ncvar_get(nc_flx, "YCRD", start = trno, count = 1),
                   ELEV = ncvar_get(nc_flx, "ELEV", start = trno, count = 1),
                   AREA = ncvar_get(nc_flx, "AREA", start = trno, count = 1),
                   SLEN = ncvar_get(nc_flx, "SLEN", start = trno, count = 1),
                   FLWR = ncvar_get(nc_flx, "FLWR", start = trno, count = 1),
                   SLAS = ncvar_get(nc_flx, "SLAS", start = trno, count = 1),
                   FLHST = ncvar_get(nc_flx, "FLHST", start = c(1,trno), count = c(-1,1)),
                   FHDUR = ncvar_get(nc_flx, "FHDUR", start = trno, count = 1)) %>%
    DSSAT::as_DSSAT_tbl(v_fmt = v_fmt_fields,
                        tier_info = tier_info_fields)

  v_fmt_planting <- c(P = "%2.0f", PDATE = "%6s", EDATE = "%6s", PPOP = "%6.0f",
                      PPOE = "%6.0f", PLME = "%6s", PLDS = "%6s", PLRS = "%6.0f", PLRD = "%6.0f",
                      PLDP = "%6.1f", PLWT = "%6f", PAGE = "%6f", PENV = "%6f", PLPH = "%6f",
                      SPRL = "%6.0f", PLNAME = "                        %-6s")

  planting_details <- tibble(P = treatments$MP,
                             PDATE = ncvar_get(nc_flx, "PDATE", start = treatments$MP, count = 1),
                             EDATE = ncvar_get(nc_flx, "EDATE", start = treatments$MP, count = 1),
                             PPOP = ncvar_get(nc_flx, "PPOP", start = treatments$MP, count = 1),
                             PPOE = ncvar_get(nc_flx, "PPOE", start = treatments$MP, count = 1),
                             PLME = ncvar_get(nc_flx, "PLME", start = c(1,treatments$MP), count = c(-1,1)),
                             PLDS = ncvar_get(nc_flx, "PLDS", start = c(1,treatments$MP), count = c(-1,1)),
                             PLRS = ncvar_get(nc_flx, "PLRS", start = treatments$MP, count = 1),
                             PLRD = ncvar_get(nc_flx, "PLRD", start = treatments$MP, count = 1),
                             PLDP = ncvar_get(nc_flx, "PLDP", start = treatments$MP, count = 1),
                             PLWT = ncvar_get(nc_flx, "PLWT", start = treatments$MP, count = 1),
                             PAGE = ncvar_get(nc_flx, "PAGE", start = treatments$MP, count = 1),
                             PENV = ncvar_get(nc_flx, "PENV", start = treatments$MP, count = 1),
                             PLPH = ncvar_get(nc_flx, "PLPH", start = treatments$MP, count = 1),
                             SPRL = ncvar_get(nc_flx, "SPRL", start = treatments$MP, count = 1),
                             PLNAME = ncvar_get(nc_flx, "PLNAME", start = c(1,treatments$MP), count = c(-1,1))) %>%
  DSSAT::as_DSSAT_tbl(v_fmt = v_fmt_planting)

  v_fmt_fertilizers <- c(F = "%2.0f", FDATE = "%6s", FMCD = "%6s", FACD = "%6s", FDEP = "%6.0f",
                         FAMN = "%6.0f", FAMP = "%6.0f", FAMK = "%6.0f", FAMC = "%6.0f",
                         FAMO = "%6.0f", FOCD = "%6s", FERNAME = " %-s")

  fertilizers <- tibble(F = ncvar_get(nc_flx, "LNFER", start = 1, count = -1),
                        FDATE = ncvar_get(nc_flx, "FDATE", start = 1, count = -1),
                        FMCD = ncvar_get(nc_flx, "FMCD", start = c(1,1), count = c(-1,-1)),
                        FACD = ncvar_get(nc_flx, "FACD", start = c(1,1), count = c(-1,-1)),
                        FDEP = ncvar_get(nc_flx, "FDEP", start = 1, count = -1),
                        FAMN = ncvar_get(nc_flx, "FAMN", start = 1, count = -1),
                        FAMP = ncvar_get(nc_flx, "FAMP", start = 1, count = -1),
                        FAMK = ncvar_get(nc_flx, "FAMK", start = 1, count = -1),
                        FAMC = ncvar_get(nc_flx, "FAMC", start = 1, count = -1),
                        FAMO = ncvar_get(nc_flx, "FAMO", start = 1, count = -1),
                        FOCD = ncvar_get(nc_flx, "FOCD", start = c(1,1), count = c(-1,-1)),
                        FERNAME = ncvar_get(nc_flx, "FERNAME", start = c(1,1), count = c(-1,-1))) %>%
    filter(F == treatments$MF) %>%
    DSSAT::as_DSSAT_tbl(v_fmt = v_fmt_fertilizers)

  v_fmt_sim_ctrl <- c(N = "%2.0f", GENERAL = " %-7s", NYERS = "%10.0f", NREPS = "%6.0f",
                      START = "%6s", SDATE = "%6s", RSEED = "%6.0f", SNAME = " %-26s",
                      SMODEL = "%-s", OPTIONS = " %-11s", WATER = "%6s", NITRO = "%6s",
                      SYMBI = "%6s", PHOSP = "%6s", POTAS = "%6s", DISES = "%6s", CHEM = "%6s",
                      TILL = "%6s", CO2 = "%6s", METHODS = " %-11s", WTHER = "%6s",
                      INCON = "%6s", LIGHT = "%6s", EVAPO = "%6s", INFIL = "%6s", PHOTO = "%6s",
                      HYDRO = "%6s", NSWIT = "%6.0f", MESOM = "%6s", MESEV = "%6s",
                      MESOL = "%6s", MANAGEMENT = " %-11s", PLANT = "%6s", IRRIG = "%6s",
                      FERTI = "%6s", RESID = "%6s", HARVS = "%6s", OUTPUTS = " %-11s",
                      FNAME = "%6s", OVVEW = "%6s", SUMRY = "%6s", FROPT = "%6.0f",
                      GROUT = "%6s", CAOUT = "%6s", WAOUT = "%6s", NIOUT = "%6s", MIOUT = "%6s",
                      DIOUT = "%6s", VBOSE = "%6s", CHOUT = "%6s", OPOUT = "%6s", PLANTING = " %-11s",
                      PFRST = "%6.0f", PLAST = "%6.0f", PH2OL = "%6.0f", PH2OU = "%6.0f",
                      PH2OD = "%6.0f", PSTMX = "%6.1f", PSTMN = "%6.1f", IRRIGATION = " %-11s",
                      IMDEP = "%6.0f", ITHRL = "%6.0f", ITHRU = "%6.0f", IROFF = "%6s",
                      IMETH = "%6s", IRAMT = "%6.0f", IREFF = "%6.0f", NITROGEN = " %-11s",
                      NMDEP = "%6.0f", NMTHR = "%6.0f", NAMNT = "%6.0f", NCODE = "%6s",
                      NAOFF = "%6s", RESIDUES = " %-11s", RIPCN = "%6.0f", RTIME = "%6.0f",
                      RIDEP = "%6.0f", HARVEST = " %-11s", HFRST = "%6.0f", HLAST = "%6.0f",
                      HPCNP = "%6.0f", HPCNR = "%6.0f")

  tier_info_sim_ctrl <- list(c("N", "GENERAL", "NYERS", "NREPS", "START", "SDATE", "RSEED",
                               "SNAME", "SMODEL"),
                             c("N", "OPTIONS", "WATER", "NITRO", "SYMBI",
                               "PHOSP", "POTAS", "DISES", "CHEM", "TILL", "CO2"),
                             c("N", "METHODS", "WTHER", "INCON", "LIGHT", "EVAPO", "INFIL",
                               "PHOTO", "HYDRO", "NSWIT", "MESOM", "MESEV", "MESOL"),
                             c("N", "MANAGEMENT", "PLANT", "IRRIG", "FERTI", "RESID", "HARVS"),
                             c("N", "OUTPUTS", "FNAME", "OVVEW", "SUMRY", "FROPT", "GROUT",
                               "CAOUT", "WAOUT", "NIOUT", "MIOUT", "DIOUT", "VBOSE",
                               "CHOUT", "OPOUT"),
                             c("N", "PLANTING", "PFRST", "PLAST", "PH2OL", "PH2OU",
                               "PH2OD", "PSTMX", "PSTMN"),
                             c("N", "IRRIGATION", "IMDEP", "ITHRL", "ITHRU", "IROFF",
                               "IMETH", "IRAMT", "IREFF"),
                             c("N", "NITROGEN", "NMDEP", "NMTHR", "NAMNT", "NCODE", "NAOFF"),
                             c("N", "RESIDUES", "RIPCN", "RTIME", "RIDEP"),
                             c("N", "HARVEST", "HFRST", "HLAST", "HPCNP", "HPCNR"))

  simulation_controls <- tibble(N = treatments$SM,
                                GENERAL = "GE",
                                SDATE = ncvar_get(nc_flx, "SDATE", start = treatments$SM, count = 1),
                                MESOL = ncvar_get(nc_flx, "MESOL", start = c(1,treatments$SM), count = c(-1,1)),
                                NYERS = ncvar_get(nc_flx, "NYERS", start = treatments$SM, count = 1),
                                NREPS = ncvar_get(nc_flx, "NREPS", start = treatments$SM, count = 1),
                                START = ncvar_get(nc_flx, "START", start = c(1,treatments$SM), count = c(-1,1)),
                                RSEED = ncvar_get(nc_flx, "RSEED", start = treatments$SM, count = 1),
                                SNAME = ncvar_get(nc_flx, "SNAME", start = c(1,treatments$SM), count = c(-1,1)),
                                SMODEL = ncvar_get(nc_flx, "SMODEL", start = c(1,treatments$SM), count = c(-1,1)),
                                OPTIONS = "OP",
                                WATER = ncvar_get(nc_flx, "WATER", start = c(1,treatments$SM), count = c(-1,1)),
                                NITRO = ncvar_get(nc_flx, "NITRO", start = c(1,treatments$SM), count = c(-1,1)),
                                SYMBI = ncvar_get(nc_flx, "SYMBI", start = c(1,treatments$SM), count = c(-1,1)),
                                PHOSP = ncvar_get(nc_flx, "PHOSP", start = c(1,treatments$SM), count = c(-1,1)),
                                POTAS = ncvar_get(nc_flx, "POTAS", start = c(1,treatments$SM), count = c(-1,1)),
                                DISES = ncvar_get(nc_flx, "DISES", start = c(1,treatments$SM), count = c(-1,1)),
                                CHEM = ncvar_get(nc_flx, "CHEM", start = c(1,treatments$SM), count = c(-1,1)),
                                TILL = ncvar_get(nc_flx, "TILL", start = c(1,treatments$SM), count = c(-1,1)),
                                CO2 = ncvar_get(nc_flx, "CO2", start = c(1,treatments$SM), count = c(-1,1)),
                                METHODS = "ME",
                                WTHER = ncvar_get(nc_flx, "WTHER", start = c(1,treatments$SM), count = c(-1,1)),
                                INCON = ncvar_get(nc_flx, "INCON", start = c(1,treatments$SM), count = c(-1,1)),
                                LIGHT = ncvar_get(nc_flx, "LIGHT", start = c(1,treatments$SM), count = c(-1,1)),
                                EVAPO = ncvar_get(nc_flx, "EVAPO", start = c(1,treatments$SM), count = c(-1,1)),
                                INFIL = ncvar_get(nc_flx, "INFIL", start = c(1,treatments$SM), count = c(-1,1)),
                                PHOTO = ncvar_get(nc_flx, "PHOTO", start = c(1,treatments$SM), count = c(-1,1)),
                                HYDRO = ncvar_get(nc_flx, "HYDRO", start = c(1,treatments$SM), count = c(-1,1)),
                                NSWIT = ncvar_get(nc_flx, "NSWIT", start = treatments$SM, count = 1),
                                MESOM = ncvar_get(nc_flx, "MESOM", start = c(1,treatments$SM), count = c(-1,1)),
                                MESEV = ncvar_get(nc_flx, "MESEV", start = c(1,treatments$SM), count = c(-1,1)),
                                MANAGEMENT = "MA",
                                PLANT = ncvar_get(nc_flx, "PLANT", start = c(1,treatments$SM), count = c(-1,1)),
                                IRRIG = ncvar_get(nc_flx, "IRRIG", start = c(1,treatments$SM), count = c(-1,1)),
                                FERTI = ncvar_get(nc_flx, "FERTI", start = c(1,treatments$SM), count = c(-1,1)),
                                RESID = ncvar_get(nc_flx, "RESID", start = c(1,treatments$SM), count = c(-1,1)),
                                HARVS = ncvar_get(nc_flx, "HARVS", start = c(1,treatments$SM), count = c(-1,1)),
                                OUTPUTS = "OU",
                                FNAME = ncvar_get(nc_flx, "FNAME", start = c(1,treatments$SM), count = c(-1,1)),
                                OVVEW = ncvar_get(nc_flx, "OVVEW", start = c(1,treatments$SM), count = c(-1,1)),
                                SUMRY = "Y",
                                FROPT = ncvar_get(nc_flx, "FROPT", start = treatments$SM, count = 1),
                                GROUT = ncvar_get(nc_flx, "GROUT", start = c(1,treatments$SM), count = c(-1,1)),
                                CAOUT = ncvar_get(nc_flx, "CAOUT", start = c(1,treatments$SM), count = c(-1,1)),
                                WAOUT = ncvar_get(nc_flx, "WAOUT", start = c(1,treatments$SM), count = c(-1,1)),
                                NIOUT = ncvar_get(nc_flx, "NIOUT", start = c(1,treatments$SM), count = c(-1,1)),
                                MIOUT = ncvar_get(nc_flx, "MIOUT", start = c(1,treatments$SM), count = c(-1,1)),
                                DIOUT = ncvar_get(nc_flx, "DIOUT", start = c(1,treatments$SM), count = c(-1,1)),
                                VBOSE = "0",
                                CHOUT = ncvar_get(nc_flx, "CHOUT", start = c(1,treatments$SM), count = c(-1,1)),
                                OPOUT = ncvar_get(nc_flx, "OPOUT", start = c(1,treatments$SM), count = c(-1,1)),
                                PLANTING = "PL",
                                PFRST = ncvar_get(nc_flx, "PFRST", start = treatments$SM, count = 1),
                                PLAST = ncvar_get(nc_flx, "PLAST", start = treatments$SM, count = 1),
                                PH2OL = ncvar_get(nc_flx, "PH2OL", start = treatments$SM, count = 1),
                                PH2OU = ncvar_get(nc_flx, "PH2OU", start = treatments$SM, count = 1),
                                PH2OD = ncvar_get(nc_flx, "PH2OD", start = treatments$SM, count = 1),
                                PSTMX = ncvar_get(nc_flx, "PSTMX", start = treatments$SM, count = 1),
                                PSTMN = ncvar_get(nc_flx, "PSTMN", start = treatments$SM, count = 1),
                                IRRIGATION = "IR",
                                IMDEP = ncvar_get(nc_flx, "IMDEP", start = treatments$SM, count = 1),
                                ITHRL = ncvar_get(nc_flx, "ITHRL", start = treatments$SM, count = 1),
                                ITHRU = ncvar_get(nc_flx, "ITHRU", start = treatments$SM, count = 1),
                                IROFF = ncvar_get(nc_flx, "IROFF", start = c(1,treatments$SM), count = c(-1,1)),
                                IMETH = ncvar_get(nc_flx, "IMETH", start = c(1,treatments$SM), count = c(-1,1)),
                                IRAMT = ncvar_get(nc_flx, "IRAMT", start = treatments$SM, count = 1),
                                IREFF = ncvar_get(nc_flx, "IREFF", start = treatments$SM, count = 1),
                                NITROGEN = "NI",
                                NMDEP = ncvar_get(nc_flx, "NMDEP", start = treatments$SM, count = 1),
                                NMTHR = ncvar_get(nc_flx, "NMTHR", start = treatments$SM, count = 1),
                                NAMNT = ncvar_get(nc_flx, "NAMNT", start = treatments$SM, count = 1),
                                NCODE = ncvar_get(nc_flx, "NCODE", start = c(1,treatments$SM), count = c(-1,1)),
                                NAOFF = ncvar_get(nc_flx, "NAOFF", start = c(1,treatments$SM), count = c(-1,1)),
                                RESIDUES = "RE",
                                RIPCN = ncvar_get(nc_flx, "RIPCN", start = treatments$SM, count = 1),
                                RTIME = ncvar_get(nc_flx, "RTIME", start = treatments$SM, count = 1),
                                RIDEP = ncvar_get(nc_flx, "RIDEP", start = treatments$SM, count = 1),
                                HARVEST = "HA",
                                HFRST = ncvar_get(nc_flx, "HFRST", start = treatments$SM, count = 1),
                                HLAST = ncvar_get(nc_flx, "HLAST", start = treatments$SM, count = 1),
                                HPCNP = ncvar_get(nc_flx, "HPCNP", start = treatments$SM, count = 1),
                                HPCNR = ncvar_get(nc_flx, "HPCNR", start = treatments$SM, count = 1)) %>%
    DSSAT::as_DSSAT_tbl(v_fmt = v_fmt_sim_ctrl,
                        tier_info = tier_info_sim_ctrl)

  filex <- list(TREATMENTS = treatments,
                CULTIVARS = cultivars,
                FIELDS = fields,
                `PLANTING DETAILS` = planting_details,
                FERTILIZERS = fertilizers,
                `SIMULATION CONTROLS` = simulation_controls) %>%
    {.[map_lgl(.,~{!is.null(.)})]}

  DSSAT::write_filex(filex,"GRID0001.CRX")

  return(invisible())
}
