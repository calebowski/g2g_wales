library(xml2)
library(data.table)

ratings_new <- read_xml("../data/g2g_data/observed_new_sites/ratingcurves_NRW_ExtraSites.xml")

curves <- xml_find_all(ratings_new, "//d1:ratingCurve", xml_ns(ratings_new))
curve_list <- lapply(curves, function(curve) {
  # Get the station ID
  loc_id <- xml_text(xml_find_first(curve, ".//d1:locationId", xml_ns(curves)))
  
  # Get all the equations for this station
  eqs <- xml_find_all(curve, ".//d1:powerEquation", xml_ns(curves))
  
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

curve_dt <- rbindlist(curve_list)

new_sites_path <- "../data/g2g_data/observed_new_sites/Non G2G Flow Sites"
new_sites_files <- list.files(new_sites_path, recursive = TRUE)



flow_list <- list()
for (file in new_sites_files){
    ob <- read_xml(file.path(new_sites_path,file))
    # 1. Get the namespaces (do NOT strip them)
    ns <- xml_ns(ob)

    # 2. Add the default namespace prefix 'd1:' to your search
    value_nodes <- xml_find_all(ob, ".//d1:Value", ns = ns)

    g2g_id <- xml_attr(
    xml_find_first(ob, ".//*[local-name()='Station']"),
    "stationReference"
    )

    curve_id <- curve_dt[G2G.ID == g2g_id]


    flow_dt <- data.table(
    G2G.ID = g2g_id,
    Date  = xml_attr(value_nodes, "date"),
    Time  = xml_attr(value_nodes, "time"),
    Level = as.numeric(xml_text(value_nodes))
    )

    flow_dt[, Flow := Inf] ## index flow col

    flow_dt[curve_id, 
            on = .(Level >= minStage, Level < maxStage), 
            `:=` (cr = i.cr, alpha = i.alpha, beta = i.beta)]

    # 2. Calculate the Flow for all 140,000 rows simultaneously
    flow_dt[, Flow := cr * (Level - alpha)^beta]

    # 3. (Optional) Remove the temporary curve parameter columns to keep your table tidy
    flow_dt[, c("cr", "Level", "alpha", "beta") := NULL]
    flow_list[[file]] <- flow_dt
}

flow_dt <- rbindlist(flow_list)

flow_wide <- dcast(flow_dt, Date + Time ~ G2G.ID,
value.var = "Flow"
)


flow_wide[, Date := as.IDate(Date)]

# Extract Year, Month, Day cleanly
flow_wide[, `:=`(
  Year  = year(Date),
  Month = month(Date),
  Day   = mday(Date)
)]

flow_wide[, Time := (seq_len(.N) - 1) * 15, by = Date]
flow_wide[, Date := NULL]
setcolorder(flow_wide, c( "Year", "Month", "Day", "Time"))

setnames(
  flow_wide,
  old = names(flow_wide)[-c(1:4)],
  new = paste0(names(flow_wide)[-c(1:4)], "_Obs")
)

fwrite(flow_wide, "../data/g2g_data/observed_new_sites/2018_2021_flows_new_sites.csv")

# flow_wide[, Time := (Time %/% 100) * 60 + (Time %% 100)]