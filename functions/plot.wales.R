plot.wales <- function(dt, wales, t.val, storm.name){

  plot_dt <- dt[[t.val]][Storm %in% storm.name,]

  p <- ggplot() +
    # LAYER 1: The Wales Shapefile Background
    # filling it with a soft grey and a thin dark grey border
    geom_sf(data = wales, fill = "#f0f0f0", color = "#8c8c8c", size = 0.4) +
    
    # LAYER 2: Your Gauge Points
    # Placed on top of the map using your Easting and Northing columns
    geom_point(data = plot_dt, 
              aes(x = G2G_Easting, y = G2G_Northing, color = Exceeded), 
              size = 2.5, 
              alpha = 0.9) +
    
    # LAYER 3: Color Setup (Red for Exceeded/Hit, Blue for Safe/False)
    scale_color_manual(
      values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728", "NA" = "#7f7f7f"),
      na.value = "#7f7f7f"
    ) +
    
    # LAYER 4: Force 1:1 scale geometry so Wales doesn't get stretched
    coord_sf(datum = 27700) + 
    
    # Theme and clean layout styling
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#e0e0e0", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    ) +
    labs(
      title = "Storm Threshold Exceedance Map",
      subtitle = "Storm: Unnamed_storm_3 | Data Source: Observed Flows",
      x = "Easting (m)",
      y = "Northing (m)",
      color = "Exceeded QT?"
    )
  return(p)
}
