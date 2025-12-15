
# Test for panelification summary app

# Packages
library(harpCore)
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinybusy)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(pals)
library(scales)
library(plotly) # Needs to be installed!
library(bslib) # page_sidebar
library(ggtext) # For ranking tibble in plot
rm(list=ls()) # clear all

# Point to where the panelification rds are stored
data_path <- shiny::getShinyOption("data_path",
                                   default = NULL)
if ((is.null(data_path)) || (!dir.exists(data_path))){
  stop("Error: Please set data_path to an existing directory.")
}

mdi          <- "No data"
# Hardcoded scores to be diplayed
rank_scores  <- c("rank_avg_rank","rank_avg_fss_rank","rank_avg_fssp_rank")
var_scores   <- c("bias","mae","rmse","Rpearson")
scores       <- c(rank_scores,var_scores)
# Hardcoded plots available
var_plots    <- c("Rank summary","Rank timeseries","Variable timeseries","Variable histogram")
rank_plots   <- c("Rank summary","Rank timeseries")
# Hardcoded percentiles for value differences
val_diff_per <- c(0,0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
# Obs filters and thresholds
obs_filters  <- list("No filter" = mdi,
                     "Mean"      = "mean",
                     "Max"       = "max",
                     "25th per"  = "25",
                     "50th per"  = "50",
                     "75th per"  = "75",
                     "90th per"  = "90",
                     "95th per"  = "95",
                     "99th per"  = "99")
obst_vals         <- c(0.1,0.5,1,2.5,5,10,15,20,25,30)
obs_gt_thresholds <- paste0(">=",obst_vals)
obs_lt_thresholds <- paste0("<=",obst_vals)
obs_thresholds    <- c(obs_gt_thresholds,obs_lt_thresholds)

# UI
ui <- page_sidebar(
  title = "Panel summary!",
  sidebar = sidebar(
    width = 450,
    open = "always",
    # Adds a spinner automatically when the server is busy (after timeout)
    shinybusy::add_busy_spinner(spin = "fading-circle",
                                position = "top-right",
                                timeout = 100,
                                height = "100px",
                                width  = "100px"),
    tags$head(
      tags$style(HTML("
      .faded {
        opacity: 0.2;
        pointer-events: none;
        transition: opacity 0.5s ease;
      }
    "))),
    shiny::fluidRow(
      column(width=6,
      shiny::selectInput('expname',
                         label = 'Case',
                         choices = "Waiting...")),
      column(width=6,
      shiny::selectInput('param',
                         label = 'Parameter',
                         choices = "Waiting...")),
    ),
    shiny::selectInput('models',
                       label = 'Model combination',
                       choices = "Waiting..."),
    div(
    id = "fade-block",
    shiny::dateRangeInput('dates',
                          label = "Select date(s)"),
    shiny::fluidRow(
      column(width=6,
      shiny::selectInput('cycles',
                         label = 'Forecast cycle(s)',
                         choices = "Waiting...",
                         multiple = T)),
      column(width=6,
      shiny::selectInput('lts',
                         label = 'Lead time(s)',
                         choices = "Waiting...",
                         multiple = T)),
    ),
    shiny::fluidRow(
      column(width=6,
      shiny::selectInput('scores',
                         label = 'Score',
                         choices = "Waiting...")),
      column(width=6,
      shiny::selectInput('plottype',
                         label = 'Type of plot',
                         choices = "Waiting...")),
    ),
    shiny::fluidRow(
      column(width=6,
             shiny::selectInput('obs_filter',
                                label   = 'Obs. filter',
                                choices = "Waiting...")),
      column(width=6,
             shiny::selectInput('obs_threshold',
                                label   = 'Obs. threshold',
                                choices = "Waiting..."))
    ),
    shiny::selectInput('val_diff_per',
                       label = 'Score difference (SD) percentile filter',
                       choices = "Waiting..."),
    shiny::uiOutput("ranking_ui"),
    shiny::checkboxInput("perc_calc",
                         label = "Calculate SD percentiles over filtered data",
                         value = T),
    shiny::checkboxInput("lt_facet",
                             label = "Facet by lead time",
                             value = F),
    shiny::checkboxInput("indicate_eq_ranks",
                         label = "Indicate equal ranks",
                         value = T),
    shiny::checkboxInput("remove_eq_ranks",
                         label = "Drop cases with equal ranks",
                         value = F),
    shiny::actionButton("plotbutton",
                        "Plot",
                        icon = shiny::icon("chart-line")),
    )
  ),
  # Output 
  shiny::textOutput(outputId = "text"),
  plotly::plotlyOutput(outputId = "plot")
)

# Server
server <- function(input, output, session) {
  
  ## Functions
  
  # Update select options
  fn_update_options <- function(input_options,
                                input_name,
                                m_var = mdi) {
    
    # Get options
    if (length(input_options) > 0){
      if (input_name == "val_diff_per") {
        input_options_named <- paste0("Top ",100-as.numeric(gsub("\\%","",names(input_options))),"%")
        input_options_named <- paste0(input_options_named," (SD>=",round(input_options,2),")")
        input_options       <- unname(input_options)
      } else if (input_name %in% c("obs_filter","obs_threshold")) {
        input_options_named <- names(input_options)
        input_options       <- unname(input_options) %>% unname() %>% unlist()
      } else {
        input_options_named <- stringr::str_to_title(gsub("_"," ",input_options))
      }
      input_options_named <- stats::setNames(as.list(input_options),input_options_named)
    } else {
      input_options <- m_var
      input_options_named <- m_var
    }
    
    # Check selected
    selec <- NULL 
    input_selected <- input[[input_name]]
    if (!is.null(input_selected[[1]])) {
      if (all(input_selected %in% input_options)){
        selec <- input_selected
      } else if (input_selected[1] == m_var) {
        if (input_options[1] != m_var) {
          selec <- input_options[1]
        } else {
          selec <- m_var
        }
      }
    } else {
      if (input_options[1] != m_var) {
        selec <- input_options[1]
      } else {
        selec <- m_var
      }
    }
    
    shiny::observe({
      shiny::updateSelectInput(
        session,
        input_name,
        choices = input_options_named,
        selected = selec
      )
    })
  }
  
  # Read rds files
  fn_read_data <- function(files_in,
                           models_to_read = NULL,
                           relabel_equal = T,
                           remove_equal  = T) {
    
    if (is.null(models_to_read)) {
      stop("Need to specify the model combination")
    } else {
      models_to_read <- strsplit(models_to_read,"-") %>% unlist()
    }
    
    # Speed things up by filtering to files which only contain all of the models_to_read
    files_in_log <- sapply(files_in, function(txt) {
      all(sapply(models_to_read, function(sub) grepl(sub, txt)))
    }) %>% unname()
    files_in <- files_in[files_in_log]

    df <- list() # Dataframe to store all data
    for (i in seq(1,length(files_in))) {
      
      # Populate for this file
      tgfile  <- file.path(data_path,input$expname,files_in[i])
      qwe     <- readRDS(tgfile)
      tdf     <- list() # A list of scores for each model for a single tgfile
      counter <- 0      # For saving to tdf
      
      # Need to have all of the requested models in the data frame. Avoids
      # having an inconsistent number of cases.  
      # Skip if not
      if (!(all(models_to_read %in% names(qwe)))) {
        next
      }
      
      # Old method
      # for (mname in names(qwe)) {
      #   qwee <- qwe[[mname]]
      #   for (score in scores) {
      #     if (score %in% names(qwee)) {
      #       
      #       if ("ranking" %in% names(qwee[[score]])) {
      #         ranking     <- qwee[[score]][["ranking"]]
      #         val         <- qwee[[score]][[score]]
      #       } else {
      #         ranking     <- qwee[[score]]
      #         val         <- ranking # For these we only have a ranking
      #       }
      #       
      #       tdf$model   <- append(tdf$model,mname)
      #       tdf$score   <- append(tdf$score,score)
      #       tdf$ranking <- append(tdf$ranking,ranking)
      #       tdf$value   <- append(tdf$value,val)
      #       
      #     }
      #   }
      # }
      
      # Loop over scores then models, allows for re-ranking of each score
      for (score in intersect(scores,names(qwe[[1]]))) {
        qtdf <- NULL # Initialise every time
        # Save time by only reading the models of interest
        for (mname in intersect(names(qwe),models_to_read)) {
          qwee <- qwe[[mname]]
          if (score %in% names(qwee)) {
            
            if ("ranking" %in% names(qwee[[score]])) {
              ranking     <- qwee[[score]][["ranking"]]
              val         <- qwee[[score]][[score]]
            } else {
              ranking     <- qwee[[score]]
              # For these use the avg_rank or fss_sum_rank
              if (score == "rank_avg_rank") {
                val       <- qwee[["avg_rank"]]
              } else if (score == "rank_avg_fss_rank") {
                val       <- qwee[["fss_sum_rank"]]
              } else if (score == "rank_avg_fssp_rank") {
                val       <- qwee[["fssp_sum_rank"]]
              } else {
                stop("ASAS")
              }
            }
            
            qtdf$model   <- append(qtdf$model,mname)
            qtdf$score   <- append(qtdf$score,score)
            qtdf$ranking <- append(qtdf$ranking,ranking)
            qtdf$value   <- append(qtdf$value,val)
            
          }
        } # Model
        
        qtdf <- as.data.frame(qtdf)
        
        # Re-rank in case models have been removed using previous loop
        new_ranks    <- rank(qtdf$ranking, ties.method = "min")
        qtdf$ranking <- new_ranks
        
        # Add information on the difference in scores across all models
        qtdf$val_diff <- max(qtdf$value) - min(qtdf$value)

        # Add to data frame for all scores. Avoid bind_rows for each score loop.
        # Using a list is much faster.
        counter        <- counter + 1
        tdf[[counter]] <- qtdf
        
      } # score
      
      # Now bind and mutate
      tdf               <- dplyr::bind_rows(tdf)
      tdf$fcdate        <- qwee[[1]][["fcdate"]]
      tdf$lead_time     <- qwee[[1]][["leadtime"]]
      tdf$ranking_label <- tdf$ranking
      tdf$ranking_label <- as.character(tdf$ranking_label)
      tdf$remove_ind    <- 0*tdf$ranking # Just to indicate to remove row
      
      # If ranks are the same, relabel. Always relabel if remove_equal is T
      if (remove_equal) {
        relabel_equal <- T
      }
      if (relabel_equal) {
        for (cs in scores) {
          if (cs %in% unique(tdf$score)) {
            
            aa <- tdf %>% dplyr::filter(score == cs)
            
            if (length(unique(aa$ranking)) != length(aa$ranking)) {
              
              if (length(unique(aa$ranking)) == 1) {
                
                rep_val <- "="
                tdf[tdf$score == cs,]$ranking_label <- rep_val
                
              } else if (length(unique(aa$ranking) == 2)) {
    
                itf <- table(aa$ranking) %>% sort(.,decreasing=T) %>% names()
                cm <- aa$model[aa$ranking == itf[1]]
                rep_val <- paste0(paste(cm,collapse = "=")," (",itf[1],")")
                tdf[(tdf$score == cs) & (tdf$model %in% cm),]$ranking_label <- rep_val
                
              } else {
                
                stop("Need to accomodate more than 3 models!")
                
              }
              
              # Remove cases where any of the models are equal. This avoids
              # having an inconsistent number of cases for different ranks.
              # This if two models are equal and the other is not, we still
              # remove this case entirely. Hence move outside of previous ifs.
              if (remove_equal) {
                tdf[tdf$score == cs,]$remove_ind <- NA
              } 
              
            }
          }
        }
      }
      
      # If obs attributes are found, add to tdf
      for (obs_var in c("obs_pers","obs_min","obs_mean","obs_max")) {
        if (obs_var %in% names(attributes(qwe))) {
          if (obs_var == "obs_pers") {
            obs_pers_vals <- attr(qwe,"obs_pers")
            for (ll in seq(1,length(obs_pers_vals))) {
              tdf[[paste0("obs_",gsub("\\%","",names(obs_pers_vals)[ll]))]] <- obs_pers_vals[ll]
            }
          } else {
            tdf[[obs_var]] <- attr(qwe,obs_var)
          }
        }
      }
      
      # Avoid binding for each tgfile, use a list instead
      #df <- dplyr::bind_rows(df,tdf)
      df[[i]] <- tdf
      
    } # file loop (over i)
    
    # Now bind df and mutate
    df <- dplyr::bind_rows(df) %>% dplyr::as_tibble()
    df <- df %>% dplyr::mutate(
      fcst_dttm = as.POSIXct(fcdate,origin = "1970-01-01", tz = "UTC"),
      lead_time = lead_time/3600) %>% dplyr::mutate(
      fcst_cycle = harpCore::as_YMDh(fcst_dttm) %>% substr(.,9,10),
      valid_dttm = fcst_dttm + lubridate::hours(lead_time)) %>%
      dplyr::select(-fcdate)
    
    return(df)
  }
  
  # Filter data based on selection
  fn_filter_data <- function(df,
                             score_select,
                             lt_select,
                             cyc_select,
                             date_select,
                             obsf_select,
                             obst_select){
    
    # Filter by score necessary
    df <- df %>% dplyr::filter(score == score_select)
    # Optional leadtime and cycle filtering
    if (!(tolower("All") %in% tolower(lt_select))) {
      df <- df %>% dplyr::filter(lead_time %in% lt_select)
    }
    if (!(tolower("All") %in% tolower(cyc_select))) {
      df <- df %>% dplyr::filter(fcst_cycle %in% cyc_select)
    }
    # Date filtering
    df <- df %>% dplyr::filter(fcst_dttm >= harpCore::as_dttm(paste0(gsub("-","",date_select[1]),"00")))
    df <- df %>% dplyr::filter(fcst_dttm <= harpCore::as_dttm(paste0(gsub("-","",date_select[2]),"23")))
    
    # Obs filtering
    if (obsf_select != mdi) {
      tgname <- paste0("obs_",obsf_select)
      if (tgname %in% names(df)) {
        if (obst_select != "N/A") {
          if (grepl(">=",obst_select,fixed = T)) {
            obst_select_num <- gsub(">=","",obst_select) %>% as.double()
            df <- df %>% dplyr::filter(!!as.symbol(tgname) >= obst_select_num)
          } else if (grepl("<=",obst_select,fixed = T)) {
            obst_select_num <- gsub("<=","",obst_select) %>% as.double()
            df <- df %>% dplyr::filter(!!as.symbol(tgname) <= obst_select_num)
          } else {
            stop("???")
          }
        }
        attributes(df)$obs_filter <- paste0(tgname,obst_select)
      }
    }
    
    return(df)
    
  }

  # Subtitle info
  fn_plot_subtitle <- function(df) {
    
    all_dtgs <- df$fcst_dttm %>% unique() %>% sort()
    all_cycs <- df$fcst_cycle %>% unique() %>% sort()
    all_lts  <- df$lead_time %>% unique() %>% sort()
    f_str <- paste0("Forecasts: ",
                    format(all_dtgs[1],"%Y-%m-%d-%H")," - ",
                    format(tail(all_dtgs,1),"%Y-%m-%d-%H"))
    c_str <- paste0("Cycle(s): ",
                    paste(all_cycs,collapse = ","))
    l_str <- paste0("Lead time(s): ",
                    paste(all_lts,collapse = ","))
    subtitle_str <- paste(f_str,c_str,l_str,sep="; ")
    
    return(subtitle_str)
  }
  
  # Plotting function to do everything
  fn_plot_summary <- function(df,
                              plot_select,
                              relabel_equal = T,
                              remove_equal  = F,
                              lt_facet = T,
                              score_diff_val = NULL,
                              ranking_val = NULL,
                              num_bins = 30) {
    
    # Do we have data?
    if (nrow(df) == 0) {
      return(list("valid" = F,
                  "text"  = "No data found after filtering!"))
    }
    
    # Drop all NA to remove equal ranks (if they have been labelled of course!)
    if (remove_equal) {
      df <- df %>% tidyr::drop_na()
    }
    # If relabel_equal is false, set ranking_label back to ranking
    if (!relabel_equal) {
      df <- df %>% dplyr::mutate(ranking_label = as.character(ranking))
    }
    
    # Hack for rank timeseries
    if (!(plot_select == "Rank timeseries")) {
      ranking_val = NULL 
    }
    
    # Score difference filtering
    if (!is.null(score_diff_val)) {
      score_diff_val <- as.numeric(score_diff_val)
      if (score_diff_val > 0) {
        df <- df %>% dplyr::filter(val_diff >= score_diff_val)
      }
    }
    
    # Do we have data?
    if (nrow(df) == 0) {
      return(list("valid" = F,
                  "text"  = "No data found after filtering!"))
    }
      
    # Save model names, colours, and all "ranking" for future use
    model_names <- unique(df$model)
    cpal_lots   <- pals::trubetskoy() %>% unname()
    cpal_lots   <- cpal_lots[-c(3)]
    rank_avail  <- unique(df$ranking) # Just integers here!
    
    # Indicate number of cases (forecast+lt)
    ncases <- df %>% dplyr::mutate("uc" = paste0(harpCore::as_YMDh(fcst_dttm),"+",lead_time)) %>%
      dplyr::select(uc) %>% dplyr::distinct() %>% nrow()
    
    # Get subtitle after filtering
    subtitle_str <- fn_plot_subtitle(df)
    subtitle_str <- paste0(subtitle_str,"; Num. cases: ",ncases)
    if ("obs_filter" %in% names(attributes(df))) {
      subtitle_str <- paste0(subtitle_str,": ",attr(df,"obs_filter"))
    }
    if (!is.null(score_diff_val)) {
      if (score_diff_val > 0) {
        subtitle_str <- paste0(subtitle_str,"; Score diff. >=",round(score_diff_val,2))
      }
    }
        
    # Set up groups for plotting
    c_groups <- c("model")
    x_group  <- "model"
    y_group  <- "percent"
    f_group  <- "ranks"
    y_breaks <- seq(0,100,25)
    ncol     <- NULL
    
    # If it is not a rank plot, switch to value
    if (!grepl("rank",tolower(plot_select))) {
      y_group  <- "value"
      y_breaks <- waiver()
    }
    # Timeseries
    if (grepl("timeseries",tolower(plot_select))) {
      c_groups <- c(c_groups,"valid_dttm")
      x_group <- "valid_dttm"
      f_group <- "model"
      ncol    <- 1
    }
    # Histogram
    if (grepl("histogram",tolower(plot_select))) {
      x_group <- "value"
      y_group <- "Frequency"
      f_group <- "model"
    }
    
    # Only not NULL for rank timeseries, hence the above applies. Hacky!
    if (!is.null(ranking_val)) {
      y_group <- "count"
      # Need to deal with equal ranks if looking at a single value
      eqr_names <- df$ranking_label[grepl(paste0("(",ranking_val,")"),
                                          df$ranking_label,
                                          fixed=T)] %>% unique()
      if (ranking_val == "1") {
        eqr_names <- c(eqr_names,"=")
      }
      ranks_to_filter <- c(ranking_val,eqr_names)
      df <- df %>% dplyr::filter(ranking_label %in% ranks_to_filter)
      # Rename models
      df <- df %>% dplyr::mutate(model = case_when(
        ranking_label %in% rank_avail ~ model,
        .default = ranking_label
      ))
      # Finally, change ranking_label back to "ranking" i.e. a single value
      # and remove duplicated rows!
      df <- df %>% dplyr::mutate(ranking_label = ranking) %>% dplyr::distinct()
    }
    
    # Add lt as a group if required
    if (lt_facet) {
      df <- df %>% dplyr::mutate(facet_label = paste0("Lt = ",lead_time,"h"))
      c_groups <- c(c_groups,"facet_label")
    }
    
    # Compute summary
    if (grepl("rank",tolower(plot_select))){ 
      
      # If only one rank, we do not need reframe
      if ((length(unique(df$ranking_label)) == 1)) {
        df_sum <- df %>% dplyr::group_by_at(c_groups) %>%
          dplyr::summarise(count = n(), .groups = "drop") %>% 
          dplyr::group_by_at(c_groups[2:length(c_groups)]) %>%
          dplyr::mutate(total_per_group = sum(count),
                        percent = 100*count/total_per_group) %>%
          dplyr::mutate(count = as.integer(count),
                        percent = round(as.double(percent),2)) %>% 
          dplyr::ungroup()
      } else {
        df_sum <- df %>% dplyr::group_by_at(c_groups) %>%
          dplyr::reframe(ranks   = names(table(ranking_label)),
                         count   = unname(table(ranking_label)),
                         percent = 100*count/sum(count)) %>%
          dplyr::mutate(count   = as.integer(count),
                        percent = round(as.double(percent),2))
      }
      
    } else {
      
      # Compute mean of value over all cases
      df_sum <- df %>% dplyr::group_by_at(c_groups) %>%
        dplyr::summarise(value   = mean(value),
                         count   = n(),
                         .groups = "drop")
      
    }
    
    # Define colours and title
    if (grepl("timeseries|histogram",tolower(plot_select)))  {
      
      cpal_names  <- unique(df_sum$model)
      cpal_onames <- setdiff(cpal_names,model_names)
      cpal_names  <- c(model_names,cpal_onames)
      cpal_cols   <- cpal_lots[1:length(cpal_names)]
      title_str   <- stringr::str_to_title(plot_select)
      
      if (grepl("rank",tolower(plot_select))) {
        title_str   <- paste0(title_str," for rank = ",ranking_val)
        max_count   <- df_sum %>% dplyr::group_by(valid_dttm) %>%
          dplyr::summarise(count=sum(count))
        y_breaks    <- seq(0,max(max_count$count))
      }
      
    } else {
      
      # Summary plots
      cpal_names <- unique(df_sum$ranks)
      cpal_onames <- setdiff(cpal_names,c("1","2","3"))
      cpal_cols <- c("#FFD700","#C0C0C0","#CD7F32")
      if (length(cpal_onames) > 0) {
        cpal_cols <- c(cpal_cols,cpal_lots[1:length(cpal_onames)])
      }
      cpal_names <- c("1","2","3",cpal_onames)
      title_str <- "Rank summary"
      
    }
    names(cpal_cols) <- cpal_names
    
    title_str <- paste0(stringr::str_to_title(unique(df$score)),": ",title_str)
    # Indicate if equal ranks are not renamed
    if (grepl("rank",tolower(plot_select))) {
      # Removal of ranks takes preference
      if (input$remove_eq_ranks) {
        title_str <- paste0(title_str," (all equal ranks are removed)")
      } else {
        if (!input$indicate_eq_ranks) {
          title_str <- paste0(title_str," (note: equal ranks not indicated)")
        }
      }
    }
    
    # Plot
    if (y_group %in% c("count","percent")) {
      
      p <- df_sum %>% 
        ggplot2::ggplot(aes(x    = get(x_group),
                            y    = get(y_group),
                            fill = forcats::fct_rev(get(f_group)),
                            text = paste0(get(x_group),": ",get(y_group)))) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = cpal_cols)
      
      if (lt_facet) {
        p <- p + ggplot2::facet_wrap(~facet_label,ncol = ncol)
      }
      
    } else {
      
      if (grepl("histogram",tolower(plot_select))) {
        
        p <- df %>%
          ggplot2::ggplot(aes(x    = get(x_group),
                              fill = forcats::fct_rev(get(f_group)),
                              text = paste0(round(ggplot2::after_stat(x),2),
                                            ": ",
                                            ggplot2::after_stat(count)))) +
          ggplot2::geom_histogram(position = "identity",
                                  alpha    = 1,
                                  bins     = num_bins) +
          ggplot2::scale_fill_manual(values=cpal_cols)
        
        if (lt_facet) {
          p <- p + ggplot2::facet_grid(model ~ facet_label, scales = "fixed")
        } else {
          p <- p + ggplot2::facet_wrap(~model, scales = "fixed", ncol = 1)
        }
        
      } else {
        
        p <- df_sum %>%
          ggplot2::ggplot(aes(x     = get(x_group),
                              y     = get(y_group),
                              color = forcats::fct_rev(get(f_group)),
                              group = 1, # Prevents geom_line from dissapearing
                              text  = paste0(get(x_group),": ",round(get(y_group),2)))) +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          ggplot2::scale_color_manual(values=cpal_cols)
        
        if (lt_facet) {
          p <- p + ggplot2::facet_wrap(~facet_label,ncol = ncol)
        }
      }
    } 
    
    text_box <- NULL
    if (f_group == "ranks") {
      p <- p +
      ggplot2::geom_text(aes(label = paste0(percent,"% (",count,")")),
                         position = ggplot2::position_stack(vjust=0.5))
      # Calculate mean over ranking (will include equal ranks unless excluded)
      # and display on the plot. Do this after plotly
      mean_ranks <- df %>% dplyr::group_by(model) %>% 
        dplyr::summarise(mean_rank = mean(ranking)) %>%
        dplyr::arrange(mean_rank)
      text_box <- paste0(
        "<b>Mean ranks:</b> ",
        paste(mean_ranks$model, ": ",
              round(mean_ranks$mean_rank,2),
              collapse = ", ")
      )
    }
    
    p <- p +
      ggplot2::labs(x     = stringr::str_to_title(x_group),
                    y     = stringr::str_to_title(y_group),
                    color = stringr::str_to_title(f_group),
                    fill  = stringr::str_to_title(f_group)) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(breaks = y_breaks,
                                  minor_breaks = NULL) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(size = 14, margin = margin(1,0,1,0)),
        plot.subtitle   = ggplot2::element_text(size = 10, margin = margin(1,0,0,0)),
        axis.text       = ggplot2::element_text(size = 10),
        axis.title      = ggplot2::element_text(size = 10, margin = margin(0,0,0,0)),
        axis.title.x    = ggplot2::element_text(margin = margin(t=15)),
        strip.text      = ggplot2::element_text(size = 10),
        legend.text     = ggplot2::element_text(size = 10),
        axis.text.x     = ggplot2::element_text(size = 8, angle = 45,vjust = 0.75)
      ) +
      ggplot2::guides(fill = guide_legend(nrow=1))
    
    # Change date labels
    if (grepl("timeseries",tolower(plot_select))) {
      num_days <- as.numeric(max(df_sum$valid_dttm) - min(df_sum$valid_dttm))
      if (num_days < 7) {
        lab_int <- 1
        br_int  <- 1
      } else if (dplyr::between(num_days,7,40)){
        lab_int <- 2
        br_int  <- 2
      } else {
        lab_int <- 7
        br_int  <- 7
      }
      all_days   <- seq(min(df_sum$valid_dttm),max(df_sum$valid_dttm),
                        by = paste0(br_int," day"))
      label_days <- seq(min(df_sum$valid_dttm),max(df_sum$valid_dttm),
                        by = paste0(lab_int," day"))
      p <- p +
        ggplot2::scale_x_datetime(
          breaks = all_days,
          labels = function(x) {
            ifelse(x %in% label_days, format(x, "%b %d"), "")  
          }
        ) 
    }
    
    return(list("p"            = p,
                "title_str"    = title_str,
                "subtitle_str" = subtitle_str,
                "valid"        = T,
                "text"         = "Not used",
                "text_box"     = text_box))
    
  }
  
  ## End of Functions
  
  # Get experiment name
  expnames <- shiny::reactive({
    list.dirs(path=data_path,full.names=F,recursive=F)
  })
  
  # Update
  shiny::observeEvent(
    expnames(),{fn_update_options(expnames(),"expname")}
  )
  
  # Get all files for this experiment
  all_files <- shiny::reactive({
    list.files(path = file.path(data_path,input$expname), pattern = ".rds")
  })

  # Get parameters
  param_avail <- shiny::reactive({
    if (length(all_files()) > 0) {
      unique(unlist(lapply(strsplit(all_files(),"-"),'[',3)))
    } else {
      mdi
    }
  })
  # Update
  shiny::observeEvent(
    param_avail(),{fn_update_options(param_avail(),"param")}
  )

  # Find the models for this parameter
  files_l1 <- shiny::reactive({
    all_files()[unlist(lapply(strsplit(all_files(),"-"),'[',3)) == input$param]
  }) 
  
  # Find the models for this parameter
  fcst_models_avail <- reactive({
    if (length(files_l1()) > 0 ){
      amodels <- unique(unlist(lapply(files_l1(), function(s) {
      parts <- unlist(strsplit(s, "-"))
      if (5 <= length(parts)) {
        qwe <- parts[5:length(parts)] %>% gsub(".rds","",.) %>% strsplit(.,"-")
      } else {
        stop("ASA")
      }
      })))
      # Start k loop from 2, no point having individual models
      if (length(amodels) > 1){
        amc <- unlist(
          lapply(2:length(amodels), function(k) combn(amodels, k, simplify = FALSE)),
          recursive = F
        ) %>% sapply(.,paste,collapse = "-") %>% rev()
      } else {
        amc <- amodels
      }
      amc
    } else {
      mdi
    }
  })
  # Update
  shiny::observeEvent(
    fcst_models_avail(),{fn_update_options(fcst_models_avail(),"models")}
  )
  
  # Find all files for these models
  # Removing this filtering, read all files available for param!
  #files_l2 <- shiny::reactive({
  #  req(input$models)
  #  files_l1()[grepl(paste0("\\+\\d{2}-",input$models,".rds"),files_l1())]
  #})
  
  # Read all the data
  df <- shiny::reactive({
    req(input$param)
    req(input$models)
    if ((length(files_l1()) > 0) && (input$models %in% fcst_models_avail())){
      # By defualt always turn on relabeling and removal options. 
      # This avoids having to read in the data again. 
      # Then the choice for relabeling and removal is done during plotting.
      # Add models to read here in order to do the re-ranking.
      fn_read_data(files_l1(),models_to_read = input$models)
    } else {
      data.frame()
    }
  })
  
  # Update date range
  shiny::observe({
    if (nrow(df()) > 0) {
      sv = format(df()$fcst_dttm,"%Y-%m-%d")[1]
      ev = tail(format(df()$fcst_dttm,"%Y-%m-%d"),1)
    } else {
      sv = NULL
      ev = NULL
    }
    shiny::updateDateRangeInput(
      session,
      "dates",
      start = sv,
      end   = ev,
      min   = sv,
      max   = ev,
    )
  })
  
  # Update cycles
  shiny::observe({
    if (nrow(df()) > 0) {
      qwe <- df()$fcst_cycle %>% unique() %>% sort()
      qwe <- c("All",qwe)
    } else {
      qwe <- mdi
    }
    fn_update_options(qwe,"cycles")
  })
  
  # Update lead times
  shiny::observe({
    if (nrow(df()) > 0) {
      qwe <- df()$lead_time %>% unique() %>% sort()
      qwe <- c("All",qwe)
    } else {
      qwe <- mdi
    }
    fn_update_options(qwe,"lts")
  })
  
  # Update scores
  shiny::observe({
    if (nrow(df()) > 0) {
      qwe <- df()$score %>% unique() %>% sort()
    } else {
      qwe <- mdi
    }
    fn_update_options(qwe,"scores")
  })
  
  # Based on score, define the available plot types
  pt_avail <- shiny::reactive({
    req(input$scores)
    if (input$scores %in% var_scores) {
      var_plots
    } else if (input$scores %in% rank_scores) {
      rank_plots
    } else {
      mdi
    }
  })
  # Update scores
  shiny::observe({
    req(input$scores)
    fn_update_options(pt_avail(),"plottype")
  })
  
  # Check if obs info exists
  shiny::observe({
    if (nrow(df()) > 0) {
      if ("obs_mean" %in% names(df())){
        qwe <- obs_filters
      } else {
        qwe <- mdi
      }
    } else {
      qwe <- mdi
    }
    fn_update_options(qwe,"obs_filter")
  })
  
  # Populate the obs thresholds
  shiny::observe({
    req(input$obs_filter)
    if (input$obs_filter == mdi) {
      qwe <- "N/A"
    } else {
      qwe <- obs_thresholds
    }
    fn_update_options(qwe,"obs_threshold",m_var = "N/A")
  })
  
  # Filter the data to selection
  df_filtered <- shiny::reactive({
    req(input$scores != mdi)
    req(nrow(df())>0)
    fn_filter_data(df(),
                   input$scores,
                   input$lts,
                   input$cycles,
                   input$dates,
                   input$obs_filter,
                   input$obs_threshold)
  })

  # Use val_diff in the data frame to compute percentiles of value difference
  shiny::observe({
    req(input$scores != mdi)
    if (input$perc_calc) {
      qwe <- df_filtered()
    } else if (!isTRUE(input$perc_calc)) {
      qwe <- df()
    }
    if (nrow(qwe)>0) {
      qwe <- qwe %>% dplyr::filter(score == input$scores)
      per_vals <- quantile(qwe$val_diff,val_diff_per)
      # First entry is 0%, replace with score diff of zero
      per_vals[1] <- 0
    } else {
      per_vals <- NULL
    }
    fn_update_options(per_vals,"val_diff_per")
  })
  
  # Ranking selection if timeseries 
  output$ranking_ui <- shiny::renderUI({
    req(input$plottype)
    if (input$plottype == "Rank timeseries"){
      shiny::selectInput('ranking_val',
                         label   = 'Ranking',
                         choices = unique(df_filtered()$ranking) %>% sort())
    } else if (input$plottype == "Variable histogram"){
      shiny::sliderInput("slider_bins",
                         "Number of bins",
                         min   = 10,
                         max   = 100,
                         value = 30,
                         step  = 10)
    } else {
      # Just a dummy output
      tagList()
    }
  })
  
  # Plot 
  shiny::observeEvent(
    input$plotbutton, {
      if (nrow(df_filtered()>0)) {
        plist <- fn_plot_summary(df_filtered(),
                                 isolate(input$plottype),
                                 relabel_equal = isolate(input$indicate_eq_ranks),
                                 remove_equal  = isolate(input$remove_eq_ranks),
                                 score_diff_val = isolate(input$val_diff_per),
                                 lt_facet = isolate(input$lt_facet),
                                 ranking_val = isolate(input$ranking_val),
                                 num_bins = isolate(input$slider_bins))
        if (plist$valid){
          p <- plotly::ggplotly(plist$p,
                           tooltip = "text") %>%
            layout(title = list(text = paste0(plist$title_str,
                                             '<br>',
                                             '<sup>',
                                             plist$subtitle_str,
                                             '</sup>'),
                                x = 0,
                                xanchor = "start"),
                   margin = list(t=100),
                   legend = list(orientation = "h",
                                 x = 0.5,
                                 xanchor = "center",
                                 y = -0.2))
          if (!is.null(plist$text_box)) {
            p <- layout(p, annotations = list(
              x = 1,  # Relative to x-axis (1 = right)
              y = 1.075,  # Relative to y-axis (1 = top)
              xref = "paper",  # Use plot area, not data scale
              yref = "paper",
              xanchor = "right",
              yanchor = "top",
              text = plist$text_box,
              showarrow = FALSE,
              font = list(size = 12),
              bordercolor = 'black',
              borderwidth = 1,
              bgcolor = 'rgba(240,240,240,0.9)',
              opacity = 0.8
            ))
          }
          output$plot <- plotly::renderPlotly({p})
          output$text <- shiny::renderText("")
        } else {
          output$plot <- plotly::renderPlotly({})
          output$text <- shiny::renderText(plist$text)
        }
      } else {
        output$plot <- plotly::renderPlotly({})
        output$text <- shiny::renderText("No data found!")
      }
  })
  
}

# Add JS to fade/unfade based on busy state
js <- "
$(document).on('shiny:busy', function() {
  $('#fade-block').addClass('faded');
});
$(document).on('shiny:idle', function() {
  $('#fade-block').removeClass('faded');
});
"

ui <- tagList(
  ui,
  tags$script(HTML(js))
)

shinyApp(ui = ui, server = server)