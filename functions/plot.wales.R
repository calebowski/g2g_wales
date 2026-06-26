library(colorspace)

# Generate 10 colors from a perceptually uniform light-to-dark blue palette
pal <- sequential_hcl(10, palette = "Blues 2")

plot.wales.simple <- function(dt, wales, t.val, storm.name, data.type = c("obs", "mod")){

  plot_dt <- dt[[t.val]][Storm %in% storm.name & Data_Type %in% data.type,]

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




plot.wales.exceed <- function(dt, wales, storm.name, data.type = c("obs", "mod")) {


  blue_to_red_10 <- c(
  "#0A2540", # 1. Deep Navy (High density)
  "#1F4068", # 2. Dark Slate Blue (High density)
  "#3B5998", # 3. Steel Blue (High density)
  "#5B517E", # 4. Muted Violet (High density)
  "#7B4365", # 5. Deep Plum (High density)
  "#9B3149", # 6. Wine Red (Low density)
  "#BA1B2D", # 7. Crimson Red (Low density)
  "#D1001C", # 8. Vivid Dark Red (Low density)
  "#A3000F", # 9. Deep Blood Red (Low density)
  "#6E0005"  # 10. Dark Maroon (Low density)
  )

  plot_dt <- dt[Storm %in% storm.name & Data_Type %in% data.type,]

   p <- ggplot() +
    # LAYER 1: The Wales Shapefile Background
    # filling it with a soft grey and a thin dark grey border
    geom_sf(data = wales, fill = "#f9f9f9", color = "#8c8c8c", size = 0.4) +
    
    # LAYER 2: Your Gauge Points
    # Placed on top of the map using your Easting and Northing columns
    geom_point(data = plot_dt, 
              aes(x = G2G_Easting, y = G2G_Northing, color = Threshold), 
              size = 2.5, 
              alpha = 0.9) +
        
   
    scale_color_manual(values = blue_to_red_10)+
    
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
      title = paste(storm.name, "QT Threshold Exceedance Map"),
      subtitle = paste("Storm: Unnamed_storm_3 | Data Source:", data.type),
      x = "Easting (m)",
      y = "Northing (m)",
      color = "Exceeded QT?"
    )
    return(p)
}


