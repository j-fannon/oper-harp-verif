 
find_cases_in_obs <- function(start_date,
                              end_date,
                              obs_by,
                              param,
                              ob_file_path,
                              ob_file_template,
                              ob_file_format,
                              ob_file_opts,
                              ob_interp_method,
                              verif_domain,
                              cases_thr,
                              cases_ltmax = 60,
                              cases_ltmin = 6,
                              cases_cyc   = seq(0,18,6),
                              cases_perc  = NULL,
                              return_obs  = FALSE) {
  
  # Option checks
  if (is.null(cases_ltmax)) {
    message("Warning: cases_ltmax not set, using 60\n")
    cases_ltmax <- 60
  }
  if (is.null(cases_ltmin)) {
    message("Warning: cases_ltmin not set, using 6\n")
    cases_ltmin <- 6
  }
  if (is.null(cases_cyc)) {
    message("Warning: cases_cyc not set, using 00, 06, 12, 18\n")
    cases_cyc <- seq(0,18,6)
  }
  
  # Generate dtgs to loop over
  dtg_vec   <- harpCore::seq_dttm(start_date,end_date,by=obs_by)
  cases_vec <- NULL
  obs_vec   <- NULL
  cases_cyc <- sprintf("%02d",as.numeric(cases_cyc))
  
  for (dtg in dtg_vec) {
    
    # Generate file
    cfile <- harpIO::generate_filenames(file_path = ob_file_path,
                                        file_date = dtg,
                                        file_template = ob_file_template)
    # Read obs
    if (file.exists(cfile)) {
      
      obs <- harpIO::read_grid(
        cfile,
        param,
        file_format = ob_file_format,
        file_format_opts = ob_file_opts,
        transformation = "regrid",
        transformation_opts = regrid_opts(new_domain = verif_domain,
                                          method = ob_interp_method)
      )

      # Is max obs above threshold?
      # TODO: obs_accumulation
      # If there are NA in the obs, then skip
      if (sum(is.na(obs)) > 0) {
        message("Warning: NA found in ",cfile,", skipping\n")
      } else {
        if (max(obs,na.rm = T) >= cases_thr){
          message(cfile," has obs above ",cases_thr,"\n")
          if (!is.null(cases_perc)) {
            obs_quant <- quantile(obs,cases_perc,names=F,na.rm = T)
          } else {
            obs_quant <- 1 # Dummy
          }
          if (obs_quant != 0) {
            # Go back ltmax hours from this to find the possible initalisation times
            cyc_oldest <- harpCore::as_dttm(dtg) - lubridate::hours(cases_ltmax)
            cyc_newest <- harpCore::as_dttm(dtg) - lubridate::hours(cases_ltmin)
            cyc_avail  <- harpCore::seq_dttm(cyc_oldest,cyc_newest,by="1h")
            lt_avail   <- seq(cases_ltmax,cases_ltmin,-1)
            cyc_touse  <- cyc_avail[substr(cyc_avail,9,10) %in% cases_cyc]
            lt_touse   <- lt_avail[substr(cyc_avail,9,10) %in% cases_cyc]
            cases_vec$dtg <- c(cases_vec$dtg,cyc_touse)
            cases_vec$lt  <- c(cases_vec$lt,lt_touse)
            if (return_obs) {
              obs_vec[[paste0("valid_",as.character(dtg))]] <- obs
            }
          } else {
            message("Max obs is above threshold but ",cases_perc," quantile is zero, skipping this valid time\n")
          }
        }
      }

    } else {
      message("Warning: Cannot find ",cfile,"\n")
    }
    
  } # dtg loop
  
  if (is.null(cases_vec)) {
    stop("ABORT: no cases found between ",start_date,"-",end_date," above ",cases_thr,"\n")
  } else {
    cases_vec <- as_tibble(cases_vec)
  }
  
  lout <- list("cases_vec" = cases_vec,
               "obs_vec"   = obs_vec)
  
  return(lout)
  
}