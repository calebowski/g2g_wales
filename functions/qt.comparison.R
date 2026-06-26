
filter.qt <- function(event, qt) {
  event <- event$obs
  g2g_ids_obs <- colnames(event)[2:ncol(event)]
  g2g_id <- gsub("_Obs", "", g2g_ids_obs)
  filtered_qt <- qt[G2G.ID %in% g2g_id,] ## filtered_qt should have same number of cols as ncol(event) - 1
  return(filtered_qt)
}




extract.peak.discharge <- function(event, qt, T) {
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

  qt_value <- qt[, get(T)]
  names(qt_value) <- qt[, G2G.ID]

  return(list(mod = max_mod, obs = max_obs, qt = qt_value))
}


compare.qt <- function(max_discharge){
    mod <- max_discharge$mod
    obs <- max_discharge$obs
    qt <- max_discharge$qt
    g2g_ids <- names(qt)
    mod_qt <- list()
    obs_qt <- list()
    for(id in g2g_ids){
        mod_qt[[id]] <- mod[grep(id, names(mod),value = TRUE)] >= qt[id]
    }
    for (id in g2g_ids){
        obs_qt[[id]] <- obs[grep(id, names(obs), value = TRUE)] >= qt[id]
    }
    return(list(mod = mod_qt, obs = obs_qt))
}
