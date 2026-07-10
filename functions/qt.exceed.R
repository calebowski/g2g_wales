
filter.qt <- function(event, qt) {
  event <- event$obs
  g2g_ids_obs <- colnames(event)[2:ncol(event)]
  g2g_id <- gsub("_Obs", "", g2g_ids_obs)
  filtered_qt <- qt[G2G.ID %in% g2g_id,] ## filtered_qt should have same number of cols as ncol(event) - 1
  return(filtered_qt)
}



## 
extract.peak.discharge <- function(event, qt = NULL, T) {
  mod <- event$mod
  obs <- event$obs
  max_mod <- c()
  for (i in colnames(mod)[2:ncol(mod)]) { ## skip the date
    max_mod[i] <-  max(mod[, ..i])
  }
  max_obs <- c()
  for (i in colnames(obs)[2:ncol(obs)]) { ## skip the date
    max_obs[i] <-  max(obs[, ..i])
  }
  #
  if(!is.null(qt)){ 
    qt_value <- qt[, get(T)]
    names(qt_value) <- qt[, G2G.ID]
    return(list(mod = max_mod, obs = max_obs, qt = qt_value))
  } else {
    return(list(mod = max_mod, obs = max_obs))
  }
}


thresh.exceed <- function(max_discharge, threshold = NULL) {
    # if (!is.null(threshold) %% !inherits(threshold, "numeric"))
    mod <- max_discharge$mod
    obs <- max_discharge$obs
    if (any(names(max_discharge) %in% "qt")){ ## works with `qt` vals.
      thresh <- max_discharge$qt
      g2g_ids <- names(thresh)
    } else if (!is.null(threshold)){ ## and works with other thresholds
      thresh <- threshold
      g2g_ids <- names(thresh)
      g2g_ids <- g2g_ids[g2g_ids %in% gsub("_Mod", "", names(mod))]
    }
    # qt <- max_discharge$qt
    mod_thresh <- list()
    obs_thresh <- list()
    for(id in g2g_ids){
        mod_thresh[[id]] <- mod[grep(id, names(mod),value = TRUE)] >= thresh[id]
    }
    for (id in g2g_ids){
        obs_thresh[[id]] <- obs[grep(id, names(obs), value = TRUE)] >= thresh[id]
    }
    return(list(mod = mod_thresh, obs = obs_thresh))
}
