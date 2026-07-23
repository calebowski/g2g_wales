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

accum.rainfall <- function(rg, cdata) {
  g2g_ids <- names(rg)[-1]
  catchment_sizes <- cdata[match(g2g_ids, G2G.ID)]$CATCHMENTSIZE
  # rainfall_m <- apply(rg[,-1], 2,function(x)  x/ 1000)
  # catchment_sizes_m <- vapply(catchment_sizes, 1, function(x) x * )
  rainfall_volume_m3 <- sweep(rg[,-1], 2, catchment_sizes * 1000, "*") ## converts to m^3
  # rainfall_volume_m <- apply(rainfall_volume, 2, function(x) x * 1000)
  cum_rainfall_volume_m3 <- as.data.table(apply(rainfall_volume_m3,2, cumsum))
  cum_rainfall_volume_m3 <- cbind(rg[,1], cum_rainfall_volume_m3) ## add date_time back
  return(cum_rainfall_volume_m3)
}


accum.river.vol <- function(event){

  river_vol_m3 <- lapply(event, function(x){
    sweep(x[,-1], 2, 15 * 60, "*" )
  })
  cum_river_volume_m3 <- lapply(vol_m3, function(x){
    as.data.table(
          lapply(as.data.table(x), function(y) {
            cs <- cumsum(replace(y, is.na(y), 0))
            cs
          })
  )})
  cum_river_volume_m3 <- Map(function(type_event, type_vol){
    cbind(type_event[,1], type_vol)
  }, event, cum_river_volume_m3)
  return(cum_river_volume_m3)
}

make.hydrograph.sim.sufi <-function(event.data, rain.data = NULL, accum.rg = NULL, accum.rv = NULL, g2g.id, pstart = NULL, pend = NULL, storm.name, cdata, qt.data, sufi, ...){

  match_call <- match.call()

  if (!inherits(g2g.id, "character")) {
    stop(match_call$g2g.id, " must be character string..\n")
  }


  if (!inherits(event.data, "list") || !all(names(event.data) %in% c("sim", "obs", "sufi"))){
    stop(match.call$event.data, "must be a list of 3 items named `sim`, `sufi`, and `obs`...\n")
  }

  if ((!is.null(pstart) || !is.null(pend)) && (!inherits(pend, "POSIXct") || !inherits(pstart, "POSIXct"))){
    stop("pstart and pend must be of class `POSIXct`\n")
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

  if(!is.null(pstart)) {
    event.data <- lapply(event.data, function(data) {
      data[DATE_TIME >= pstart]
    })
  }
  if(!is.null(pend)) {
    event.data <- lapply(event.data, function(data) {
      data[DATE_TIME <=  pend]
    })
  }

  if (!is.null(rain.data)){
    rain.data <- rain.data[[storm.name]][, .(DATE_TIME, precip = get(g2g.id))]
    if (!is.null(pstart)) rain.data <- rain.data[rain.data$DATE_TIME >= pstart, ]
    if (!is.null(pend))   rain.data <- rain.data[rain.data$DATE_TIME <= pend, ]
    # rain.data[, rain_ymin := ymax - precip * rain_scale]
    # rain.data[, rain_ymax := ymax]
  } 

  if (!is.null(accum.rg)){
    accum.rg <- accum.rg[[storm.name]][, .(DATE_TIME, precip = get(g2g.id))]
    if (!is.null(pstart)) accum.rg <- accum.rg[accum.rg$DATE_TIME >= pstart, ]
    if (!is.null(pend))   accum.rg <- accum.rg[accum.rg$DATE_TIME <= pend, ]
  } 

  if (!is.null(accum.rv)) {
    accum.rv_obs <- accum.rv[[storm.name]]$obs[, list(DATE_TIME, vol = get(paste0(g2g.id, "_Obs")))]
    accum.rv_sim <- accum.rv[[storm.name]]$sim[, list(DATE_TIME, vol = get(paste0(g2g.id, "_Mod")))]
    accum.rv_sufi <- accum.rv[[storm.name]]$sufi[, list(DATE_TIME, vol = get(paste0(g2g.id, "_Mod")))]
    if (!is.null(pstart)) accum.rv <- accum.rv[accum.rv$DATE_TIME >= pstart, ]
    if (!is.null(pend))   accum.rv <- accum.rv[accum.rv$DATE_TIME <= pend, ]
  }

          
  max_flow <- max(c(event.data$obs[[paste0(g2g.id, "_Obs")]], event.data$sim[[paste0(g2g.id, "_Mod")]], event.data$sufi[[paste0(g2g.id, "_Mod")]]), na.rm = TRUE)
  max_vol  <- max(c(accum.rv_obs$vol, accum.rv_sim$vol, accum.rv_sufi$vol))

  scale_fac <- max_flow / max_vol

 
  # if (!is.null(river_name))

  p_flow <- ggplot() +
  # geom_rect(
  #   data = rain.data,
  #   aes(
  #     xmin = DATE_TIME - 1800,
  #     xmax = DATE_TIME + 1800,
  #     ymin = rain_ymin,
  #     ymax = rain_ymax
  #   ),
  #   fill = "#4C78A8",
  #   alpha = 0.35,
  #   inherit.aes = FALSE
  # ) + 
  geom_line(data = event.data$obs, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Obs")), color = "Observed"), linewidth = 0.8) +

  
   geom_line(data = event.data$sufi, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Mod")), color = "Sufi")) +

            # Modelled
  geom_line(data = event.data$sim, 
            aes(x = DATE_TIME, y = get(paste0(g2g.id, "_Mod")), color = "Sim")) +

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
  
  # scale_y_continuous(
  #   limits = c(0, ymax),
  #   name = expression(Flow ~ (m^3 ~ s^-1)),
  #   sec.axis = sec_axis(
  #     # trans = ~ (ymax - .) / rain_scale,
  #     name = "Rainfall (mm)"
  #   )
  # ) +
    
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
  theme(legend.position = c(0.35, 0.8),
        # legend.justification = c(1, 0.5),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.title    = element_blank(),
        legend.text = element_text(size = 10),
        # aspect.ratio = 0.8,
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 15)
  )


  if (!is.null(accum.rv)){
    p_flow <- p_flow + 
    geom_line(data = accum.rv_obs, 
    aes(DATE_TIME, vol * scale_fac, colour = "Observed"),  linetype = "dashed") +

    geom_line(data = accum.rv_sim, aes(DATE_TIME, vol * scale_fac, colour = "Sim"),  linetype = "dashed") +

    geom_line(data = accum.rv_sufi, aes(DATE_TIME, vol * scale_fac, colour = "Sufi") ,  linetype = "dashed") +

    # scale_color_manual(values = c(
    # "Sufi" = "#016fd6",
    # "Sim" = "#f0140c",
    # "Observed" = "#1a1a1a")) +

    scale_y_continuous(
            name = expression(Flow ~ (m^3 ~ s^-1)),
            sec.axis = sec_axis(
              ~ . / scale_fac,
              name = expression("Cumulative river volume ("*m^3*")")
            )
          ) 
  }

  p_flow <- p_flow + list(...)

  if (!is.null(rain.data)){
          
      max_rain <- max(rain.data$precip, na.rm = TRUE)
      max_vol  <- max(accum.rg$precip, na.rm = TRUE)

      scale_fac <- max_rain / max_vol

   
      p_rain <- ggplot() +
          geom_col(
            data = rain.data,
            aes(DATE_TIME, precip),
            fill = "#4C78A8"
          ) +
          geom_line(
            data = accum.rg,
            aes(DATE_TIME, precip * scale_fac),
            colour = "red",
            linewidth = 0.8
          ) +
          scale_y_continuous(
            name = "Rainfall (mm/15 min)",
            sec.axis = sec_axis(
              ~ . / scale_fac,
              name = expression("Cumulative rainfall volume ("*m^3*")")
            )
          ) +
          labs(x = "") +
          theme_bw() +
          scale_x_datetime(date_labels = "%d-%m-%Y")

        
    p_flow <- p_rain / p_flow
  }
  return(p_flow)
}


## convert flows to m3 convert to 15 and then by 60
## flip it around.

