
#' Event should be a list of names mod and obs, containing data.tables of columns corresponding to g2g ids
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
    return(errors)
}