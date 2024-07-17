# build_history() ---------------------------------------------------------

build_history <- function(df_hist, df_new, kpi_number) {
  if (season == "spring") {
    table(df_hist$kpi, df_hist$fin_year)
    
    print("Don't add to the history file. Move along to next step")
    
  } else {
    
    if (season == "autumn") {
      # initial tests
      build_history_checks(df_hist, df_new, kpi_number)
      
      # create filename based on KPI inputted
      filenames <- build_history_filenames(kpi_number)
      
      
      # Save historical backup --------------------------------------------------
      # save a backup of df_hist
      # if kpi_report_year[2] already present, means it's already been updated this analysis round
      if(!kpi_report_years[2] %in% df_hist$fin_year & !kpi_number == "1.4"){
        # write backup file
        query_write_rds(df_hist, paste0(hist_path, filenames$filename_bckp))
        # change permissions to give the group read/write
        Sys.chmod(paste0(hist_path, filenames$filename_bckp),
                  mode = "664", use_umask = FALSE)
        
        print("Backup of historical database written.")
      } else {
        print("Backup already created for this analysis round.")
      }
      
      # format df_new for inclusion
      build_history_format_df_new(kpi_number, df_new)
      
      print("Table of df_new$kpi, df_new$fin_year:")
      print(table(df_new$kpi, df_new$fin_year))
      
      
      # New historical database -------------------------------------------------
      
      # create new historical database
      new_hist_db <- build_history_create_new_hist(df_hist, df_new, kpi_number)
      
      print("Table of new_hist_db$kpi, new_hist_db$fin_year:")
      print(table(new_hist_db$kpi, new_hist_db$fin_year))
      
      # write new hist_db
      query_write_rds(new_hist_db, paste0(hist_path, filenames$filename_hist))
      # change permissions to give the group read/write
      Sys.chmod(paste0(hist_path, filenames$filename_hist),
                mode = "664", use_umask = FALSE)
      
      print("You made history! Proceed.")
      
    } else {
      
      stop("Season is not 'spring' or 'autumn'. Go check your calendar!")
    }
  }
}



# build_history_checks() --------------------------------------------------

build_history_checks <- function(df_hist, df_new, kpi_number){
  stopifnot(
    "KPI inputted is not included in accepted list, see documentation" = kpi_number %in% c("1.1-1.3", "1.4", "2", "3")
  )
  stopifnot(
    "no 'season' variable defined in global environment" = exists("season", envir = globalenv())
  )
  stopifnot(
    "no 'hist_path' variable defined in global environment" = exists("hist_path", envir = globalenv())
  )
  stopifnot(
    "no 'kpi_report_years' variable defined in global environment" = exists("kpi_report_years", envir = globalenv())
  )
  stopifnot(
    "no 'fy_list' variable defined in global environment" = exists("fy_list", envir = globalenv())
  )
  stopifnot(
    "no 'hb_list' variable defined in global environment" = exists("hb_list", envir = globalenv())
  )
  if (kpi_number %in% c("1.1-1.3", "1.4")) {
    stopifnot(
      "no 'year2' variable defined in global environment" = exists("year2", envir = globalenv())
    )
  }
}

# build_history_filenames() -----------------------------------------------

build_history_filenames <- function(kpi_number) {
  historical <- "/aaa_kpi_historical_"
  bckp <- "_bckp.rds"
  reg <- ".rds"
  
  theme <- paste0("theme", as.numeric(substr(kpi_number, 1, 1))+1)
  filename_bckp <- paste0(historical, theme, bckp) # backup file
  filename_hist <- paste0(historical, theme, reg) # new historical db
  
  return(list(filename_bckp = filename_bckp, filename_hist = filename_hist, theme = theme, historical = historical, bckp = bckp, reg = reg))
}


# build_history_format_df_new() -------------------------------------------


build_history_format_df_new <- function(kpi_number, df_new) {
  
  if(kpi_number == "1.1-1.3"){
    df_new <- df_new |>
      dplyr::filter(kpi != "KPI 1.1 Sept coverage",
                    fin_year != year2)
  }
  else {
    df_new <- df_new |>
      dplyr::filter(fin_year == kpi_report_years[3])
  }
}


build_history_create_new_hist <- function(df_hist, df_new, kpi_number) {
  
  new_hist_db <- add_new_rows(df1 = df_hist, df2 = df_new, fin_year, kpi) |>
    dplyr::mutate(fin_year = forcats::fct_relevel(fin_year, c(fy_list)),
                  hbres = forcats::fct_relevel(hbres, c(hb_list)))
  
  if(kpi_number == "1.1-1.3"){
    new_hist_db <- new_hist_db |>
      dplyr::mutate(
        kpi = forcats::fct_relevel(
          kpi, c("KPI 1.1", "KPI 1.1 Scotland SIMD",
                 "KPI 1.1 Scotland SIMD Sept coverage",
                 "KPI 1.2a", "KPI 1.2a Sept coverage",
                 "KPI 1.2b", "KPI 1.3a Scotland SIMD",
                 "KPI 1.3a Sept coverage", "KPI 1.3a HB SIMD",
                 "KPI 1.3b Scotland SIMD", "KPI 1.3b HB SIMD",
                 "KPI 1.4a", "KPI 1.4b")))
  }
  
  new_hist_db <- new_hist_db |>
    dplyr::arrange(kpi, fin_year, hbres)
  
  return(new_hist_db)
}
