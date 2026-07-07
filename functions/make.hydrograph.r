make.hydrograph.simple <-function(event.data, g2g.id, storm.name, cdata, qt.data){

  match_call <- match.call()

  if (!inherits(g2g.id, "character")) {
    stop(match_call$g2g.id, " must be character string..\n")
  }


  # if (!inherits(event.data, "list") || !names(event.data) %in%  c("obs", "mod")){
  #   stop(match_call$event.data, " must be a data.frame...\n")
  # }

  river_name <- cdata[G2G.ID == g2g.id, ]$River.Name
  site_name <- cdata[G2G.ID == g2g.id, ]$Site.Name.
  catchment_size <- cdata[G2G.ID == g2g.id, ]$CATCHMENTSIZE
  qt <- qt.data[[storm.name]][G2G.ID == g2g.id, ]
  qmed <- qt$qmed
  max_discharge <- max(c(event.data$mod[,get(paste0(g2g.id, "_Mod"))], event.data$obs[, get(paste0(g2g.id, "_Obs"))]))
  qt_only_vals <-  qt[, .(q5, q10, q25, q50, q75, q100, q200, q250, q1000)]
  highest_qt_exceeded_index <- which(max_discharge > qt_only_vals)
  qt_vals <- qt_only_vals[, highest_qt_exceeded_index, with = FALSE]
  qt_df <- data.frame(qt = names(qt_vals), value = unlist(qt_vals))

  # if (!is.null(river_name))

  p <- ggplot() +
  # Modelled
  geom_line(data = event.data$mod, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Mod")), color = "Model")) +

  # Observed 
  geom_line(data = event.data$obs, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Obs")), color = "Observed"), linewidth = 0.8) +

  geom_hline(yintercept = qmed, color = "darkgreen", linetype = "dotted", linewidth = 0.4) +

  annotate(
    "text", 
    x = min(event.data$obs$DATE_TIME, na.rm = TRUE),
    y = qmed, 
    label = "qmed",
    vjust = -0.8,
    hjust = 0,
    color = "darkgreen",
    fontface = "bold"
  ) +
  
  geom_hline(data = qt_df, aes(yintercept =qt_df$value), inherit.aes = FALSE,color = "blue", linetype = "dashed", linewidth = 0.4) +
  
  geom_text(
      data = qt_df,
      aes(
        x = min(event.data$obs$DATE_TIME, na.rm = TRUE),
        y = qt_df$value,
        label = qt_df$qt
      ),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = -0.8,
      colour = "blue",
      fontface = "bold"
    ) +
  # Labels and Scale adjustments
    labs(
      title = paste(storm.name, "at", g2g.id),
      subtitle = paste("River:", river_name, "Site:", site_name, "Catchment size:", catchment_size),
      x = "",
      y = expression(Flow ~ (m^3 ~ s^-1))) +
  scale_color_manual(values = c("Model" = "red", "Observed" = "black")) +
  # Themes and Legend placement
  theme_bw() +
  scale_x_datetime(date_labels = "%d-%m-%Y") +
  theme(legend.position = c(0.8, 0.8),
        legend.title    = element_blank(),
        aspect.ratio = 0.8,
        plot.title = element_text(size = 9),
        plot.subtitle = element_text(size = 7),
  )

  return(p)
}


make.hydrograph.sim.sufi <-function(event.data, g2g.id, storm.name, cdata, qt.data, sufi){

  match_call <- match.call()

  if (!inherits(g2g.id, "character")) {
    stop(match_call$g2g.id, " must be character string..\n")
  }


  # if (!inherits(event.data, "list") || !names(event.data) %in%  c("obs", "mod")){
  #   stop(match_call$event.data, " must be a data.frame...\n")
  # }

  river_name <- cdata[G2G.ID == g2g.id, ]$River.Name
  site_name <- cdata[G2G.ID == g2g.id, ]$Site.Name.
  catchment_size <- cdata[G2G.ID == g2g.id, ]$CATCHMENTSIZE
  su <- sufi[G2G.ID == g2g.id]$SU
  fi <- sufi[G2G.ID == g2g.id]$FI
  su <- ifelse(su == 1, "SU", "")
  fi <- ifelse(fi == 1, "FI", "")
  qt <- qt.data[G2G.ID == g2g.id, ]
  qmed <- qt$qmed
  max_discharge <- max(c(event.data$sim[,get(paste0(g2g.id, "_Mod"))], event.data$obs[, get(paste0(g2g.id, "_Obs"))], event.data$sufi[,get(paste0(g2g.id, "_Mod"))]), na.rm=TRUE) ## extract max discharge
  qt_only_vals <-  qt[, .(q5, q10, q25, q50, q75, q100, q200, q250, q1000)]
  highest_qt_exceeded_index <- which(max_discharge > qt_only_vals)
  qt_vals <- qt_only_vals[, highest_qt_exceeded_index, with = FALSE]
  qt_df <- data.frame(qt = names(qt_vals), value = unlist(qt_vals))

  # if (!is.null(river_name))

  p <- ggplot() +
  # Modelled
  geom_line(data = event.data$sim, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Mod")), color = "Sim")) +
  
   geom_line(data = event.data$sufi, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Mod")), color = "Sufi")) +

  # Observed 
  geom_line(data = event.data$obs, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Obs")), color = "Observed"), linewidth = 0.8) +

  geom_hline(yintercept = qmed, color = "darkgreen", linetype = "dotted", linewidth = 0.4) +

  annotate(
    "text", 
    x = min(event.data$obs$DATE_TIME, na.rm = TRUE),
    y = qmed, 
    label = "qmed",
    vjust = -0.8,
    hjust = 0,
    color = "darkgreen",
    fontface = "bold"
  ) +
  
  geom_hline(data = qt_df, aes(yintercept =qt_df$value), inherit.aes = FALSE,color = "black", linetype = "dashed", linewidth = 0.4) +
  
  geom_text(
      data = qt_df,
      aes(
        x = min(event.data$obs$DATE_TIME, na.rm = TRUE),
        y = qt_df$value,
        label = qt_df$qt
      ),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = -0.8,
      colour = "black",
      fontface = "bold"
    ) +
  # Labels and Scale adjustments
    labs(
      title = paste(storm.name, "at", g2g.id),
      subtitle = paste("River:", river_name, "Site:", site_name, "\nCatchment size:", catchment_size, "Site config:", paste0(su, fi)),
      x = "",
      y = expression(Flow ~ (m^3 ~ s^-1))) +
      scale_color_manual(values = c(
      "Sufi" = "#016fd6",
      "Sim" = "#f0140c",
      "Observed" = "#1a1a1a")) +
  # Themes and Legend placement
  theme_bw() +
  scale_x_datetime(date_labels = "%d-%m-%Y") +
  theme(legend.position = c(0.8, 0.8),
        legend.title    = element_blank(),
        legend.text = element_text(size = 10),
        aspect.ratio = 0.8,
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 15)
  )
  return(p)
}


