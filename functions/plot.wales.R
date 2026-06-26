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

  # 1. THE NAMED VECTOR: This absolutely maps the threshold to the color.
  # If a plot only has q5 and q1000, it will perfectly pull DodgerBlue and Black.
  # THE NAMED VECTOR: High-Saturation Hazard Scale
  qt_colors <- c(
    "qmed"  = "#003366", # Navy Blue
    "q1"    = "#005ce6", # Royal Blue
    "q5"    = "#0099ff", # Vibrant Light Blue
    "q10"   = "#00cccc", # Bright Teal / Cyan (Replaces the pale blue)
    "q25"   = "#ffcc00", # Solid Gold (Replaces the near-white)
    "q50"   = "#ff9900", # Vibrant Orange
    "q75"   = "#ff6600", # Dark Orange
    "q100"  = "#ff0000", # Pure Red
    "q200"  = "#cc0000", # Dark Red
    "q250"  = "#990000", # Maroon
    "q1000" = "#000000"  # Pitch Black (Extreme)
  )

  # Filter the data based on function arguments
  plot_dt <- dt[Storm %in% storm.name & Data_Type %in% data.type,]

  p <- ggplot() +
    # LAYER 1: The Wales Shapefile Background
    geom_sf(data = wales, fill = "#f9f9f9", color = "#8c8c8c", size = 0.4) +
    
    # LAYER 2: Your Gauge Points
    geom_point(data = plot_dt, 
               aes(x = G2G_Easting, y = G2G_Northing, color = Threshold), 
               size = 2.5, 
               alpha = 0.9) +
        
    # LAYER 3: Apply the locked-in color scale
    # drop = FALSE ensures the legend still shows all categories if you want a consistent legend, 
    # but usually you just want to map the colors cleanly.
    scale_color_manual(values = qt_colors) +
    
    # LAYER 4: Force 1:1 scale geometry
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
      subtitle = paste("Storm:", storm.name, "| Data Source:", data.type),
      x = "Easting (m)",
      y = "Northing (m)",
      color = "Exceeded QT?"
    )
    
  return(p)
}
