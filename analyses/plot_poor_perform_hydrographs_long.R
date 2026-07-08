source("../functions/make.hydrograph.R")


if (!dir.exists("../data/figures/long_bad_hydrographs")){
  dir.create("../data/figures/long_bad_hydrographs")
}
path <- "../data/figures/long_bad_hydrographs"


years <- c("2018", "2019", "2020", "2021")
pstart <- list()
for (i in 1:3){
  pstart[[i]] <- years[i]
}
pend <- list()
for (i in 1:3){
  pend[[i]] <- years[i+1]
}



for (g2g_id in bad_in_both){
    sim <- mod_ts$sim[, .SD, .SDcols = c("DATE_TIME", paste0(g2g_id, "_Mod"))]
    sufi <- mod_ts$sufi[, .SD, .SDcols = c("DATE_TIME", paste0(g2g_id, "_Mod"))]
    obs <- obs_ts[, .SD, .SDcols = c("DATE_TIME", paste0(g2g_id, "_Obs"))]
    event <- list(sim = sim, sufi = sufi, obs =obs)
    stats[[g2g_id]] <- list()
    stats[[g2g_id]]$sim <- get.stats(mod = event$sim, obs = event$obs)
    stats[[g2g_id]]$sufi <- get.stats(mod = event$sufi, obs = event$obs)
    site_stats <- stats[[g2g_id]]


    stats_text <- sprintf(
    "Performance (Sim | SUFI)\nKGE: %.2f | %.2f\nBias: %.1f%% | %.1f%%\nR^2: %.2f | %.2f",
    site_stats$sim$KGE, site_stats$sufi$KGE,
    site_stats$sim$perc_bias, site_stats$sufi$perc_bias,
    site_stats$sim$R2, site_stats$sufi$R2
    )
    plots <- list()
    for (i in seq_along(pend)) {
      plots[[i]] <- make.hydrograph.sim.sufi(
        event,
        g2g.id = g2g_id,
        storm.name = "",
        cdata = wales_cdata,
        qt.data = filtered_qt[[1]],
        sufi = sufi_data,
        pstart = as.POSIXct(paste(pstart[[i]], "1", "1", sep = "-"), tz = "GMT"),
        pend = as.POSIXct(paste(pend[[i]], "1", "1", sep = "-"), tz = "GMT"),
        theme(,
        legend.title    = element_blank(),
        legend.text = element_text(size = 10),
        # aspect.ratio = 0.8,
        plot.title = element_blank(),
        plot.subtitle = element_blank())
      )
    }

    combined <- wrap_plots(plots, ncol = 1, guides = "collect") +
      plot_annotation(
        title = g2g_id,
        subtitle = stats_text
      ) &
      theme(legend.position = "bottom")

    ggsave(
      filename = file.path(path, paste0(g2g_id, "__2018_2022_hydrograph.png")),
      plot = combined,
      width = 12,
      height = 10,
      units = "in",
      dpi = 300
    )
}