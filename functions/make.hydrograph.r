make.hydrograph <-function(event_data, g2g.id, storm.name, cdata){

  match_call <- match.call()

  if (!inherits(g2g.id, "character")) {
    stop(match_call$g2g.id, " must be character string..\n")
  }

  # if (!inherits(event_data, "list") || !names(event_data) %in%  c("obs", "mod")){
  #   stop(match_call$event_data, " must be a data.frame...\n")
  # }

  river_name <- wales_cdata[G2G.ID == g2g_id, ]$River.Name
  # if (!is.null(river_name))

  p <- ggplot() +
  # Modelled
  geom_line(data = event_data$mod, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Mod")), color = "Model")) +

  # Observed 
  geom_line(data = event_data$obs, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Obs")), color = "Observed"), linewidth = 0.8) +

  # Labels and Scale adjustments
    labs(
      title = paste(storm.name, "Hydrograph at g2g site:", g2g.id),
      subtitle = paste("River:", river_name),
      x = "Date",
      y = "Streamflow") +
  scale_color_manual(values = c("Model" = "red", "Observed" = "black")) +
  # Themes and Legend placement
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        legend.title    = element_blank())

  return(p)
}
