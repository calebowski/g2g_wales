get.stats <- function(mod, obs, sstart = ISOdatetime(1066,1,1,0,0,0), send = ISOdatetime(2999,1,1,0,0,0), bias_correct = FALSE, removeStart = NA, removeEnd = NA, excludeDF = NULL, exclField = "exclude") {
  
  sstart = max(min(obs$DATE_TIME, na.rm = TRUE), min(mod$DATE_TIME, na.rm = TRUE), sstart)
  send   = min(max(obs$DATE_TIME, na.rm = TRUE), max(mod$DATE_TIME, na.rm = TRUE), send)
  
  obs   = obs[obs$DATE_TIME >= sstart & obs$DATE_TIME <= send, ]
  mod = mod[mod$DATE_TIME >= sstart & mod$DATE_TIME <= send, ]
  
  # if (!is.na(removeStart) & !is.na(removeEnd)) {
  #   obs   = obs[obs$DATE_TIME <= removeStart | obs$DATE_TIME >= removeEnd, ]
  #   mod = mod[mod$DATE_TIME <= removeStart | mod$DATE_TIME >= removeEnd, ]
  # }
  
  # if (!is.null(excludeDF)) {
  #   message("Subsetting times with excludeDF field: ", exclField) # Fixed printp() -> message()
  #   if (!(nrow(obs) == nrow(mod) && all(obs$DATE_TIME == mod$DATE_TIME))) {
  #     stop("get_stats: check times here - excludeDF")
  #   }
  #   dateTimes = as.Date(obs$DATE_TIME)
    
  #   dates_to_exclude = excludeDF$date[ excludeDF[[exclField]] == TRUE ]
  #   m_excludeDF      = dateTimes %in% dates_to_exclude
    
  # } else {
  #   m_excludeDF = rep(FALSE, nrow(obs))
  # }
  
  if (nrow(obs) != nrow(mod)) {
    stop("Number of timesteps in mod not equal to obs")
  }

  sites <- names(obs)[!names(obs) %in% c("DATE_TIME", "hr", "Step", "Year", "Month", "Day", "Time")]
  sites <- gsub("_Obs$", "", sites) # Ensure we only strip it from the absolute end

  statsDF = data.frame(G2G.ID = character(0), perc_bias = numeric(0), r = numeric(0), R2 = numeric(0), KGE = numeric(0), KGE_sqrt = numeric(0), MSE = numeric(0), RMSE = numeric(0), MAPE = numeric(0), n = numeric(0))
  
  for (site in sites) {

    obs_col <- paste0(site, "_Obs")
    mod_col <- paste0(site, "_Mod") 
    
    if (!(mod_col %in% names(mod))) {
      warning(paste("Model data missing for site:", site, "- Skipping."))
      next
    }
    
    ob  = obs[[obs_col]]
    mod = mod[[mod_col]]
    
    mod[mod < 0] = NA
    ob[ob < 0]   = NA
    
    m   <- is.na(ob) | is.na(mod) 
    ob  = ob[!m]
    mod = mod[!m]
    
    if (length(ob) != length(mod)) stop("unequal lengths after NA removal") # Fixed Stop() -> stop()
    if (length(ob) == 0) next # Skip calculating stats if all data was NA
    
    # perc_bias 
    mean_ob   = mean(ob)
    mean_mod  = mean(mod)
    bias      <- mean_mod - mean_ob
    perc_bias <- (bias / mean_ob) * 100
    
    # bias correct mod?
    if (bias_correct) {
      message("Warning - bias correct applied")
      mod      = mod - mean_mod + mean_ob
      mean_mod = mean(mod)
    }
    
    # correlation coeff
    r = cor(ob, mod)
    
    # R2 stat (NSE)
    r2 <- NA
    n_sq_sigma_ob = sum((ob - mean_ob)^2)
    SE            = sum((ob - mod)^2)
    if (n_sq_sigma_ob > 0) r2 = 1 - (SE / n_sq_sigma_ob) 
    
    # calc *MODIFIED* Kling-Gupta efficiency
    sd_ob  = sd(ob)
    sd_mod = sd(mod)
    KGE    = (r - 1)^2 + ((sd_mod / mean_mod) / (sd_ob / mean_ob) - 1)^2 + (mean_mod / mean_ob - 1)^2
    KGE    = 1 - sqrt(KGE)
    
    # calc 'Kling-Gupta efficiency' of square-rooted flows
    mod_sqrt = sqrt(mod)
    ob_sqrt  = sqrt(ob)
    
    r_sqrt        = cor(ob_sqrt, mod_sqrt)
    sd_ob_sqrt    = sd(ob_sqrt)
    sd_mod_sqrt   = sd(mod_sqrt)
    mean_ob_sqrt  = mean(ob_sqrt)
    mean_mod_sqrt = mean(mod_sqrt)
    
    KGE_sqrt = (r_sqrt - 1)^2 + ((sd_mod_sqrt / mean_mod_sqrt) / (sd_ob_sqrt / mean_ob_sqrt) - 1)^2 + (mean_mod_sqrt / mean_ob_sqrt - 1)^2
    KGE_sqrt = 1 - sqrt(KGE_sqrt)
    
    # MSE
    MSE = SE / length(ob)
    
    # MAPE (Formula technically computes NMAE)
    MAPE = 100 * mean(abs(mod - ob)) / mean_ob
    
    # n
    nn = length(ob)
    
    # Save results - using nrow() + 1 is cleaner for dataframes than using rownames
    statsDF[nrow(statsDF) + 1, ] = list(site, perc_bias, r, r2, KGE, KGE_sqrt, MSE, sqrt(MSE), MAPE, nn)
  }
  
  return(statsDF)
}