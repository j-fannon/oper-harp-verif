#!/usr/bin/env Rscript

library(harp)
library(Rgrib2)
library(hdf5r)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(here)
library(yaml)
library(lubridate)
rm(list=ls())

####################
### LOAD CONFIGS ###
####################

if (!interactive()) {
  
args <- commandArgs(trailingOnly=TRUE)

if (length(args) == 0){
	run_case   <- NULL 
	yaml_file  <- "./panel_configs/panelification.yml"
	start_date <- NULL
	end_date   <- NULL
} else if (length(args) == 1){
	run_case   <- args[[1]]
	yaml_file  <- "./panel_configs/panelification.yml"  
	start_date <- NULL
	end_date   <- NULL
} else if (length(args) == 2) {
	run_case   <- args[[1]]
	yaml_file  <- args[[2]]
	start_date <- NULL
	end_date   <- NULL
} else if (length(args) == 4) {
  run_case   <- args[[1]]
  yaml_file  <- args[[2]]
  start_date <- args[[3]]
  end_date   <- args[[4]]
} else if (length(args) == 5) {
  run_case         <- args[[1]]
  yaml_file        <- args[[2]]
  start_date       <- args[[3]]
  end_date         <- args[[4]]
  pcp_accum_period <- args[[5]] # Option to allow pcp_accum_period from CLI
} else {
  stop("Incorrect number of arguments")
}

} else {
  
  run_case   <- "prec_verif"
  yaml_file  <- "spatial_verif/panelification/panel_configs/panelification.yml"
  start_date <- NULL
  end_date   <- NULL
}

print(run_case)
print(yaml_file)

configs   <- yaml.load_file(here(yaml_file))
message("Use configs from file: ", here(yaml_file))

if (is.null(run_case)){
	cfg <- configs$verify_case[[1]]  
	message("No specific verify_case was provided.
		Running default which is the first entry of verify_case in config file.")
} else {
	cfg <- configs$verify_case[[run_case]]
	message("Run verification for: ", run_case)
}

if (is.null(cfg)){
	message("ERROR: selected verify_case is not specified in ", yaml_file)
}

def_param <- cfg$def_param 
lead_time <- cfg$lead_time 
models    <- configs$models
if ((is.null(def_param)) || (is.null(lead_time)) || (is.null(models))) {
  stop("def_param, lead_time, and models should be set")
}
# Logic for multiple dates and finding cases
cases_vec <- NULL
if ((is.null(start_date)) || (is.null(end_date))){
  start_date <- cfg$start_date
  end_date   <- cfg$end_date
  fcst_by    <- cfg$fcst_by
  if ((is.null(start_date)) || (is.null(end_date)) || (is.null(fcst_by))) {
    # Single case
    dtg_vec <- cfg$init_time
  } else {
    dtg_vec <- seq_dttm(start_date,end_date,by = fcst_by)
  }
} else {
  fcst_by    <- cfg$fcst_by
  find_cases <- cfg$find_cases
  if (!isTRUE(find_cases)) {
    if (!is.null(fcst_by)) {
      dtg_vec <- seq_dttm(start_date,end_date,by = fcst_by)
    } else {
      stop("Need fcst_by in config")
    }
  } else {
    obs_by    <- cfg$obs_by
    cases_thr <- cfg$cases_thr
    if (is.null(cases_thr) || (is.null(obs_by))){
      stop("Need cases_thr and obs_by in config")
    } else {
      message("Now try to find cases in period ",start_date,"-",end_date,
              " every ",obs_by," with threshold ",cases_thr)
      # Call cases_function
      source(here("spatial_verif/panelification/scripts/find_cases_in_obs.R"))
      # Source the definition file to get obs settings. Requires "model" 
      # and "init_time", so set something (although not actually important here)
      model <- models[1]
      init_time <- "2025010100"
      definition_file <- configs[[model]][[def_param]]$definition_file
      source(file.path(here("spatial_verif/panelification/panel_configs"), definition_file))
      cases_out <- find_cases_in_obs(start_date,
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
                                     cases_ltmax = cfg$cases_ltmax,
                                     cases_ltmin = cfg$cases_ltmin,
                                     cases_cyc   = cfg$cases_cyc,
                                     cases_perc  = cfg$cases_perc,
                                     return_obs  = F
                                     )
      cases_vec <- cases_out$cases_vec
    }
  }
}

# If cases_vec was not set from the cases function, merge dtg_vec and leadtime
# together to create an array of cases
if (is.null(cases_vec)) {
  if (is.null(dtg_vec)) {
    stop("dtg_vec not specified!")
  } else {
    cases_vec           <- expand.grid(dtg_vec,lead_time) %>% as_tibble()
    colnames(cases_vec) <- c("dtg","lt")
    cases_vec$dtg       <- as.character(cases_vec$dtg)
  }
}

message("Using the following for cases_vec:\n")
print(cases_vec)

definition_file_plt <- file.path(here("spatial_verif/panelification/panel_configs"), configs$plt_config[[def_param]])

#######################################################
### READ ALL DATA into verif_data and verif_fields ####
#######################################################

# Loop over cases_vec
models_input <- models
number_valid <- 0
for (ii in seq(1,nrow(cases_vec))) {

# Reset models for each case
models       <- models_input
verif_data   <- vector("list", length(models))
verif_fields <- vector("list", length(models))
names(verif_data)   <- models
names(verif_fields) <- models 
models_to_use       <- NULL
caption_str         <- NULL
  
init_time <- cases_vec$dtg[ii]
lead_time <- cases_vec$lt[ii]

for (model in models){

  definition_file <- configs[[model]][[def_param]]$definition_file
  source(file.path(here("spatial_verif/panelification/panel_configs"), definition_file))

  message("ob_file_opts: ")
  print(ob_file_opts)
  message("ob_file_template: ", ob_file_template)
  message("model:  ", model)
  message("fcdate: ", init_time)

  verif <- verify_spatial(
    dttm	      = init_time, 
    fcst_model        = model,
    parameter         = param,
    lead_time         = lead_time,
    fc_file_path      = fc_file_path,
    fc_file_template  = fc_file_template,
    fc_accumulation   = fc_accumulation,
    fc_file_format    = fc_file_format,
    fc_file_opts      = fc_file_opts,
    fc_interp_method  = fc_interp_method,
    fc_param_defs     = fc_param_defs,
    ob_file_path      = ob_file_path,
    ob_file_template  = ob_file_template,
    ob_file_format    = ob_file_format,
    ob_file_opts      = ob_file_opts,
    ob_interp_method  = ob_interp_method,
    ob_accumulation   = ob_accumulation,
    verif_domain      = verif_domain,
    sqlite_path       = sqlite_path,
    sqlite_file       = sqlite_file,
    thresholds        = thresholds,
    percentiles       = percentiles,
    window_sizes      = window_sizes,
    scores            = scores,
    qc_check_obs_NA   = T,
    qc_check_percentiles = T,
    return_data       = return_data,
    return_fields     = return_fields
  )
  fc_tmp <- list(fcfield = tibble::tibble(
				  valid_dttm = verif_date,
				  parameter  = param,
				  lead_time  = lead_time,
				  fcdate     = init_time,
				  !!as.name(model) := geolist(verif$fcfield),
				  units      = prm_units
				  ) 
  )
  class(fc_tmp) <- "harp_fcst"
  ob_tmp <- list(obfield = tibble::tibble(
				  valid_dttm = verif_date,
				  parameter  = param,
				  !!as.name(ob_name) := geolist(verif$obfield),
				  units      = prm_units
				  ) 
  )
  class(ob_tmp) <- "harp_analysis"

  verif$fcfield      <- NULL
  verif$obfield      <- NULL
  # verif$verif_domain <- NULL

  # Only add if something is found
  remove_model <- T
  for (cn in setdiff(names(verif),"qc_df")) {
    if (!is.null(verif[[cn]])) {
      remove_model <- F
      break
    }
  }
  if (remove_model) {
    message("Removed model ",model," due to missing data!")
    # Add debug message
    if (!is.null(verif[["qc_df"]])) {
      if (nrow(verif[["qc_df"]]) == 1) {
        if (is.null(caption_str)) {
          caption_str <- paste0("Error message for ",model,": ",
                                verif[["qc_df"]]$qc_ind)
        } else {
          caption_str <- paste0(caption_str,
                                ", Error message for ",model,": ",
                                verif[["qc_df"]]$qc_ind)
        }
      }
    }
  } else {
    verif_data[[model]]   <- verif
    verif_fields[[model]] <- fc_tmp
    verif_fields[[ob_name]] <- ob_tmp
    models_to_use <- c(models_to_use,model)
  }
}

if (length(models_to_use) == 0) {
  message("No valid data found for",init_time,"+",lead_time,", skipping")
  next
}
number_valid <- number_valid + 1
models       <- models_to_use
verif_data   <- verif_data[names(verif_data) %in% models]
verif_fields <- verif_fields[names(verif_fields) %in% c(models,ob_name)]

verif_fields$verif_domain <- verif_domain

message("verif_data: ")
print(verif_data)
message("verif_fields: ")
print(verif_fields)


########################
### RANK THE MODELS ####
########################

source(here("spatial_verif/panelification/scripts/panel_utils.R"))
source(here("spatial_verif/panelification/scripts/panel_ranking_functions.R"))

verif_data <- main_ranking(verif_data)

########################
####### PLOTTING #######
########################

source(here("spatial_verif/panelification/scripts/panel_plotting_functions.R"))

png_sep   <- "-"
panel_str <- "panel"
if (exists("case_descrip")) {
  plot_name_str <- paste(panel_str,case_descrip,sep = png_sep)
} else {
  plot_name_str <- panel_str
}
plot_name_common <- paste(plot_name_str,
                          param,
                          paste0(format(as_dttm(init_time), format="%Y%m%d%H%M+"),
                                 stringr::str_pad(lead_time,2,pad = "0")),
                          sep = png_sep)
plot_name <- paste(plot_name_common,
                   paste0(models,collapse = png_sep),
                   sep = png_sep)
plot_name <- paste0(plot_name,".png")
if (!exists("plot_path")){ 
  plot_path = here("spatial_verif/panelification/PLOTS")
} else {
  if (!dir.exists(plot_path)){
    plot_path = here("spatial_verif/panelification/PLOTS")
  } else {
    # Save in CASE/YYYYMM folder corresponding to the valid date
    verif_ymd <- harpCore::as_ymd(verif_date)
    plot_path_ext <- ""
    if (exists("case_descrip")) {
      plot_path_ext <- case_descrip 
    }
    plot_path_ext <- file.path(plot_path_ext,
                               paste0(substr(verif_ymd,1,6)))
    if ((nchar(basename(plot_path)) == 6) && (!grepl("\\D",basename(plot_path)))) {
      # To avoid going down rabbit holes of directories
      message("No appending to",plot_path,"\n")
    } else {
      plot_path <- file.path(plot_path,plot_path_ext)
      if (!dir.exists(plot_path)) {
        dir.create(plot_path,recursive = T)
      }
    }
  }
}

main_plotting(verif_data      = verif_data,
	      verif_fields    = verif_fields,
	      ob_name         = ob_name,
	      param           = param,
	      plt_definitions = definition_file_plt,
	      plot_path       = plot_path,
	      plot_name       = plot_name,
	      caption_str     = caption_str)

# Before saving, add obs percentiles as an attribute for reference
if (!exists("add_obs_info")) {
  add_obs_info <- T
}
if (add_obs_info) {
  obs_pers <- c(0,0.1,0.25,0.5,0.75,0.9,0.95,0.99)
  obs_pers <- quantile(verif_fields[[ob_name]]$obfield[[ob_name]][[1]],
                       probs = obs_pers,
                       na.rm = T)
  attributes(verif_data)$obs_pers <- obs_pers
  attributes(verif_data)$obs_min  <- min(verif_fields[[ob_name]]$obfield[[ob_name]][[1]],na.rm=T)
  attributes(verif_data)$obs_mean <- mean(verif_fields[[ob_name]]$obfield[[ob_name]][[1]],na.rm=T)
  attributes(verif_data)$obs_max  <- max(verif_fields[[ob_name]]$obfield[[ob_name]][[1]],na.rm=T)
}

# Save the data associated with this panel
data_name <- gsub("panel_","paneldata_",plot_name,fixed = T)
data_name <- gsub(".png",".rds",data_name,fixed = T)
saveRDS(object = verif_data, file = file.path(plot_path,data_name))

# Quick plot FSS only 
if (configs$save_additional_plt$FSS){
  for (model in models){
    verif    <- verif_data[[model]]
    title    <- paste("Model: ", model, ", Param: ", param)
    subtitle <- paste0("Period: ", format(init_time, format="%Y-%m-%d %H:%M"), 
                         " + ", lead_time)
    plot_name <- paste(gsub(panel_str,"FSS",plot_name_common,fixed = T),
                       model,
                       sep = png_sep)
    plot_name <- paste0(plot_name,".png")
    quick_plot_FSS(verif,
		   title     = title,
		   subtitle  = subtitle,
		   plot_path = plot_path,
		   plot_name = plot_name)
  }
}


# Quick plot fields only
if (configs$save_additional_plt$fields){
  source(definition_file_plt)
  title    <- paste(ob_name, ", Param: ", param)
  subtitle <- paste0("Period: ", format(verif_date, format="%Y-%m-%d %H:%M"))
  plot_name <- paste(gsub(panel_str,"field",plot_name_common,fixed = T),
                     ob_name,
                     sep = png_sep)
  plot_name <- paste0(plot_name,".png")
  
  plot_panel_field(verif_fields[[ob_name]]$obfield,
  		 ob_name,
  		 title    = title,
  		 subtitle = subtitle,
  		 breaks   = breaks,
  		 palette  = palette,
  		 plot_path = plot_path,
  		 plot_name = plot_name)
  
  for (model in models){
  	title    <- paste("Model: ", model, ", Param: ", param)
  	subtitle <- paste0("Period: ", format(init_time, format="%Y-%m-%d %H:%M"),
                         " + ", lead_time)
  	plot_name <- paste(gsub(panel_str,"field",plot_name_common,fixed = T),
  	                   model,
  	                   sep = png_sep)
  	plot_name <- paste0(plot_name,".png")
  	
  	plot_panel_field(verif_fields[[model]]$fcfield,
  			 model,
  			 title    = title,
  			 subtitle = subtitle,
  			 breaks   = breaks,
  			 palette  = palette,
  			 plot_path = plot_path,
  			 plot_name = plot_name)
  }
}
message("Finished case ",ii," / ",nrow(cases_vec))

} # Cases loop

message("Finished. There were ",number_valid," valid cases from a possible ",nrow(cases_vec))
