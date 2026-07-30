
# Event should be a list of names mod and obs, containing data.tables of columns corresponding to g2g ids
peak.magnitude.error <- function(event) {
    peak_discharge <- extract.peak.discharge(event)
    mod <- peak_discharge$mod
    obs <- peak_discharge$obs
    g2g_ids <- gsub("_Obs", "", names(obs))
    pme <- list()
    for(id in g2g_ids){
        mod_Q <- as.numeric(mod[grep(id, names(mod),value = TRUE)])
        obs_Q <- as.numeric(obs[grep(id, names(obs),value = TRUE)])
        pme[[id]] <- ((mod_Q - obs_Q) / obs_Q) * 100
    }
    return(pme)
}



filter.qt <- function(event, qt) {
  event <- event$obs
  g2g_ids_obs <- colnames(event)[2:ncol(event)]
  g2g_id <- gsub("_Obs", "", g2g_ids_obs)
  filtered_qt <- qt[G2G.ID %in% g2g_id,] ## filtered_qt should have same number of cols as ncol(event) - 1
  return(filtered_qt)
}



## extracts peak discharge, can return QT as a value if inputted.
## qt is the qt_dt, with one col for g2g id and the others qt values. T needs to be a character string, eg "qmed" corresponding to colnames of qt_dt
extract.peak.discharge <- function(event, qt = NULL, T) {
  mod <- event$mod
  obs <- event$obs
  max_mod <- c()
  for (i in colnames(mod)[2:ncol(mod)]) { ## skip the date
    max_mod[i] <-  max(mod[, ..i])
  }
  max_obs <- c()
  for (i in colnames(obs)[2:ncol(obs)]) { ## skip the date
    max_obs[i] <-  max(obs[, ..i], na.rm = TRUE)
  }
  #
  if(!is.null(qt)){ 
    qt_value <- qt[, get(T)] ## extract the qt column wanted specified by T character string
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


## pod far with tolerance

## note
pod.far.tol <- function(max_discharge, threshold){
    mod <- max_discharge$mod
    obs <- max_discharge$obs
    g2g_ids <- names(threshold)

    pod_far_tol <- data.table()
    for (id in g2g_ids) {

      layer <- NULL

      thresh <- threshold[id]
      obs_val <- obs[gsub("_Obs", "", names(obs)) == id]
      mod_val <- mod[gsub("_Mod", "", names(mod)) == id]

    if (length(obs_val) != 1 || length(mod_val) != 1 || length(thresh) != 1 ||
        !is.finite(obs_val) || !is.finite(mod_val) || !is.finite(thresh)) {
      
      layer <- data.table(G2G.ID = id, outcome = "missing data")
      
    } else {


      obs_exceed <- obs_val >= thresh
      mod_exceed <- mod_val >= thresh

      if (obs_exceed && mod_exceed) {
        layer <- data.table(G2G.ID = id, outcome = "hit")

      } else if (!obs_exceed && !mod_exceed) {
        layer <- data.table(G2G.ID = id, outcome = "correct rejection")

      } else if (!mod_exceed && obs_exceed) {
        if (mod_val >= thresh * 0.8) {
          layer <- data.table(G2G.ID = id, outcome = "near miss")
        } else {
          layer <- data.table(G2G.ID = id, outcome = "miss")
        }

      } else if (!obs_exceed && mod_exceed) {
        if (obs_val >= thresh * 0.8) {
          layer <- data.table(G2G.ID = id, outcome = "close false alarm")
        } else {
          layer <- data.table(G2G.ID = id, outcome = "false alarm")
        }
      }
    }

      if (!is.null(layer)) {
        pod_far_tol <- rbind(pod_far_tol, layer)
      }
      }
        return(pod_far_tol)
}



## hit: obs exceeds threshold, mod exceeds threshold
## near miss: obs exceeds threshold, mod within 20%
## miss: obs exceeds threshold, mod does not hit threshold
## false alarm: mod exceeds threshold, obs below 20% of threshold
## close false alarm: mod exceeds threshold, obs within 20%
