# library(colorspace)

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


plot.wales.exceed <- function(dt, wales, storm.name, data.type = c("obs", "mod")) {

  qt_colors <- c(
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
               aes(x = G2G_Easting, y = G2G_Northing, color = Threshold), 
               size = 2.5, 
               alpha = 0.9) +
        
    ## scale color=ur
    scale_color_manual(values = qt_colors) +
    
    ## Force 1:1 scale geometry so Wales doesn't get stretched
    coord_sf(datum = 27700) + 
    
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#e0e0e0", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    ) +
    labs(
      title = paste(storm.name, "QT Threshold Exceedance Map"),
      subtitle = paste("Storm:", storm.name, "| Data Source:", data.type),
      x = "Easting (m)",
      y = "Northing (m)",
      color = "Exceeded QT?"
    )
    
  return(p)
}
