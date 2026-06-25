make.hydrograph <-function(event_data, catchment){

  match_call <- match.call()

  if (!inherits(catchment, "character")) {
    stop(match_call$catchment, " must be character string..\n")
  }

  if (!inherits(event_data, "data.frame")){
    stop(match_call$event_data, " must be a data.frame...\n")
  }

  p <- ggplot() +
  # Modelled
  geom_line(data = storm, 
            aes(x = DATE_TIME, y = get(paste0(catchment, "_Mod")), color = "Model")) +

  # Observed 
  geom_line(data = storm, 
            aes(x = DATE_TIME, y = get(paste0(catchment, "_Obs")), color = "Observed"), linewidth = 0.8) +

  # Labels and Scale adjustments
  xlab("Date") +
  ylab("Streamflow (CMS)") +
  scale_color_manual(values = c("Model" = "red", "Observed" = "black")) +

  # Themes and Legend placement
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        legend.title    = element_blank())

  return(p)
}
