make.hydrograph <-function(event.data, g2g.id, storm.name, cdata, qt.data){

  match_call <- match.call()

  if (!inherits(g2g.id, "character")) {
    stop(match_call$g2g.id, " must be character string..\n")
  }

  # if (!inherits(event.data, "list") || !names(event.data) %in%  c("obs", "mod")){
  #   stop(match_call$event.data, " must be a data.frame...\n")
  # }

  river_name <- wales_cdata[G2G.ID == g2g.id, ]$River.Name
  qt <- qt.data[[storm.name]][G2G.ID == g2g.id, ]
  qmed <- qt$qmed
  max_discharge <- max(c(event.data$mod[,get(paste0(g2g.id, "_Mod")) ], event.data$obs[,get(paste0(g2g.id, "_Obs")) ]))
  qt_only_vals <-  qt[, c(5:14), with = FALSE]
  highest_qt_exceeded_index <- max(which(max_discharge > qt_only_vals))
  qt_val <- qt_only_vals[, highest_qt_exceeded_index, with = FALSE]

  # if (!is.null(river_name))

  p <- ggplot() +
  # Modelled
  geom_line(data = event.data$mod, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Mod")), color = "Model")) +

  # Observed 
  geom_line(data = event.data$obs, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Obs")), color = "Observed"), linewidth = 0.8) +

  geom_hline(yintercept = qmed, color = "darkgreen", linetype = "dotted") +

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


  geom_hline(yintercept = qt_val[[1]], color = "blue", linetype = "dashed") +
  
  # LAYER 2: The Text Label
  annotate(
    "text", 
    x = min(event.data$obs$DATE_TIME, na.rm = TRUE), 
    y = qt_val[[1]], 
    label = names(qt_val)[1], 
    vjust = -0.8, 
    hjust = 0, 
    color = "blue", 
    fontface = "bold"
  ) +
  # Labels and Scale adjustments
    labs(
      title = paste(storm.name, "Hydrograph at g2g site:", g2g.id),
      subtitle = paste("River:", river_name),
      x = "Date",
      y = "Streamflow") +
  scale_color_manual(values = c("Model" = "red", "Observed" = "black")) +
  # Themes and Legend placement
  theme_bw() +
  scale_x_datetime(date_labels = "%d-%m-%Y") +
  theme(legend.position = c(0.8, 0.8),
        legend.title    = element_blank())

  return(p)
}
