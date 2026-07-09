library(xml2)
library(data.table)


# 1. Read the XML document
doc <- read_xml("../data/ratingcurves_NRW_May2026.xml")

# 2. Extract all rating curves
curves <- xml_find_all(doc, "//d1:ratingCurve", xml_ns(doc))

# 3. Build a flat lookup table
curve_list <- lapply(curves, function(curve) {
  # Get the station ID
  loc_id <- xml_text(xml_find_first(curve, ".//d1:locationId", xml_ns(doc)))
  
  # Get all the equations for this station
  eqs <- xml_find_all(curve, ".//d1:powerEquation", xml_ns(doc))
  
  # Extract attributes into a data.table
  rbindlist(lapply(eqs, function(eq) {
    data.table(
      G2G.ID = loc_id,
      minStage = as.numeric(xml_attr(eq, "minStage")),
      maxStage = as.numeric(xml_attr(eq, "maxStage")),
      cr = as.numeric(xml_attr(eq, "cr")),
      alpha = as.numeric(xml_attr(eq, "alpha")),
      beta = as.numeric(xml_attr(eq, "beta"))
    )
  }))
})

# Combine all stations into one master lookup table
rating_table <- rbindlist(curve_list)
level_thresh <- fread("../data/nrw_level_threshold.csv")
level_thresh_alert <- level_thresh[ threshold.type == "Flood Alert", .( threshold.level = min(threshold.level)),  by = .(G2G.ID)]

level_thresh_alert <- level_thresh[ threshold.type == "Flood Alert", .(threshold.level),  by = .(G2G.ID)]

level_thresh_alert[, join_level := threshold.level] ## make a duplicate column for joining and one spare for putting it in

# Perform a non-equi join to match the stage to the correct equation bracket
flow_thresholds_alert <- rating_table[level_thresh_alert, 
                          on = .(G2G.ID, minStage <= join_level, maxStage > join_level), 
                          nomatch = NULL]

# Calculate the discharge (handling any potential negative bases before the power operation)
flow_thresholds_alert[, flow_thresholds_alert := cr * (threshold.level - alpha)^beta]

# Clean up the output
final_thresholds <- flow_thresholds_alert[, .(G2G.ID, threshold.level, flow_thresholds_alert)]
fwrite(final_thresholds, "../data/nrw_alert_threshold_convert_flow.csv")
