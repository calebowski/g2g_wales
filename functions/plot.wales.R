library(viridis)

# Generate 10 colors from a perceptually uniform light-to-dark blue palette
# pal <- sequential_hcl(10, palette = "Blues 2")

# plot.wales.simple <- function(dt, wales, t.val, storm.name, data.type = c("obs", "mod")){

#   plot_dt <- dt[[t.val]][Storm %in% storm.name & Data_Type %in% data.type,]

#   p <- ggplot() +
#     # LAYER 1: The Wales Shapefile Background
#     # filling it with a soft grey and a thin dark grey border
#     geom_sf(data = wales, fill = "#f0f0f0", color = "#8c8c8c", size = 0.4) +
    

#     geom_point(data = plot_dt, 
#               aes(x = G2G_Easting, y = G2G_Northing, color = Exceeded), 
#               size = 2.5, 
#               alpha = 0.9) +
    
#     scale_color_manual(
#       values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728", "NA" = "#7f7f7f"),
#       na.value = "#7f7f7f"
#     ) +
    
#     # Force 1:1 scale geometry so Wales doesn't get stretched
#     coord_sf(datum = 27700) + 
    
#     theme_minimal() +
#     theme(
#       panel.grid.major = element_line(color = "#e0e0e0", size = 0.2),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(face = "bold", size = 14),
#       legend.position = "right"
#     ) +
#     labs(
#       title = "Storm Threshold Exceedance Map",
#       subtitle = "Storm: Unnamed_storm_3 | Data Source: Observed Flows",
#       x = "Easting (m)",
#       y = "Northing (m)",
#       color = "Exceeded QT?"
#     )
#   return(p)
# }


plot.wales.exceed <- function(dt, wales, storm.name, data.type = c("obs", "sim", "sufi")) {

  if (data.type %in% c("sim", "sufi")){
    data_source <- data.type
    data.type  <- "mod"
  } else if (data.type== "obs"){
    data_source <- data.type
  }


  qt_colours <- c(
    "None" = NA,
    "qmed"  = "#003366", 
    "q1"    = "#005ce6", 
    "q5"    = "#0099ff", 
    "q10"   = "#00cccc", 
    "q25"   = "#ffcc00", 
    "q50"   = "#ff9900", 
    "q75"   = "#ff6600", 
    "q100"  = "#ff0000", 
    "q200"  = "#cc0000", 
    "q250"  = "#990000", 
    "q1000" = "#570303"  
  )

  plot_dt <- dt[Storm %in% storm.name & Data_Type %in% data.type,]

  p <- ggplot() +
    # layer the wales map
    geom_sf(data = wales, fill = "#f9f9f9", color = "#8c8c8c", size = 0.4) +
    
    # place g2g site points
    geom_point(data = plot_dt, 
               aes(x = G2G_Easting, y = G2G_Northing, fill = Threshold),
               size = 2.5, 
               alpha = 0.9,
               stroke = 0.6,
               shape = 21,
               color = "black") +
        
    ## scale color=ur
    scale_fill_manual(
    values = qt_colours,
    na.value = NA
    ) +
    
    ## Force 1:1 scale geometry so Wales doesn't get stretched
    coord_sf(datum = 27700) + 
    
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#e0e0e0", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
      ) +
    labs(
      # title = paste(storm.name, "QT Threshold Exceedance Map"),
      title = paste("Data source:", data_source),
      x = "Easting (m)",
      y = "Northing (m)",
      color = "Exceeded QT?"
    )
    
  return(p)
}



plot.wales.pme <- function(dt, wales, storm.name) {

  plot_dt <- dt[Storm %in% storm.name]
  # max_pme <- max(dt$pme, na.rm = TRUE)
  # min_pme <- min(dt$pme, na.rm = TRUE)


  p <- ggplot() +
    # layer the wales map
    geom_sf(data = wales, fill = "#f9f9f9", color = "#8c8c8c", size = 0.4) +
    
    # place g2g site points
    geom_point(data = plot_dt, 
               aes(x = G2G_Easting, y = G2G_Northing, color = pme), 
               size = 5.0, 
               alpha = 0.9) +
        
    ## scale color=ur

    
    scale_colour_gradient2(
      low = "#0c82f8",      # strong blue (underestimate — IMPORTANT)
      mid = "#2F2F2F",      # dark neutral (clear + visible)
      high = "#f80623",     # strong red (overestimate)
      midpoint = 0,
    limits = c(min(dt$pme, na.rm = TRUE), quantile(dt$pme, 0.98, na.rm = TRUE)),
      trans = "pseudo_log",
      oob = scales::squish,
      breaks = function(x) {
        # x is a vector of your current limits: c(min, max)
        # This dynamically creates breaks based on your actual data limits
        val_min <- round(x[1])
        val_max <- round(x[2])
        
        # Create a neat set of intermediate breaks (adjust these numbers to fit your typical PME range)
        intermediates <- c(-75, -50, -25, 0, 25, 50, 100, 250)
        
        # Combine the min, intermediates, and max, keeping only values within the limits
        valid_breaks <- intermediates[intermediates >= val_min & intermediates <= val_max]
        unique(sort(c(val_min, valid_breaks, val_max)))
      },
      
      # 2. Keep the custom labels for the + sign
      labels = function(x) {
        ifelse(x > 0, paste0("+", x), as.character(x))
      }
    ) +

    
    
    
  # scale_colour_viridis_c(
  #   option = "C",
  #   limits = c(min(dt$pme), quantile(dt$pme, 0.99, na.rm = TRUE)),
  #   oob = scales::squish
  # ) +

    
    ## Force 1:1 scale geometry so Wales doesn't get stretched
    coord_sf(datum = 27700) + 
    
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#e0e0e0", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 20),
      legend.position = "right",
      legend.text = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    labs(
      title = paste(storm.name, "Peak Magnitude Error"),
      x = "",
      y = "",
      color = "PME %"
    ) +
    guides(color = guide_colorbar(
      title.position = "top", 
      barheight = unit(3, "in"), 
      barwidth = unit(0.2, "in"),
      ticks.linewidth = 1,
      draw.ulim = TRUE, 
      draw.llim = TRUE
    ))
    
  return(p)
}

## utility funtion to plot a single point based on g2g

## g2g.id can be a vector of strings or single string
plot.single.point <- function(g2g.id, cdata, wales, fill.variable = NULL, ...) {

  match_call <- match.call()
  if(!is.null(fill.variable) && (!ncol(fill.variable) == 2 || !colnames(fill.variable)[1] == "G2G.ID")){
    stop(match_call$fill.variable, "must be a data.frame/datatable of 2 columns (named `G2G.ID` and some other variable name\n)" )
  }

  northing_easting <- cdata[G2G.ID %in% g2g.id,
                             .(G2G.ID, G2G.Easting, G2G.Northing, CATCHMENTSIZE)]

  if (!is.null(fill.variable)) {
    # stopifnot(nrow(fill.variable) == length(g2g.id))
    northing_easting <- merge(northing_easting, fill.variable, by = "G2G.ID", all.x = TRUE)
  }
  fill.var <- colnames(fill.variable)[2]

  p <- ggplot() +
    geom_sf(data = wales, fill = "#f9f9f9", color = "#8c8c8c", size = 0.4) +
    geom_point(
      data = northing_easting,
      aes(
        x = G2G.Easting,
        y = G2G.Northing,
        size = CATCHMENTSIZE,
        fill = if (is.null(fill.variable)) "black" else get(fill.var)
      ),
      alpha = 0.9,
      shape = 21,
      color = "black"
    ) +
    coord_sf(datum = 27700) +
    theme_minimal() +
    labs(
      title = paste(g2g.id, "location in Wales"),
      x = "Easting (m)",
      y = "Northing (m)",
      size = "Catchment size",
      fill = if(!is.null(fill.variable)) colnames(fill.variable)[2] else NULL
    )

  if (!is.null(fill.variable)) {
    p <- p + scale_fill_viridis_c()
  }

  p
}
