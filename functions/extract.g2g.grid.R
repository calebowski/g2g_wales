
## adapted from `griddedData_class_jrw.py` script converted to R
read.ascii <- function(file_name) {
  cat("*opening ascii: ", file_name, "\n")
  

  header_lines <- readLines(file_name, n = 6, warn = FALSE)
  
  parsed_header <- strsplit(trimws(header_lines), "\\s+")
  
  nX <- as.integer(parsed_header[[1]][2])
  nY <- as.integer(parsed_header[[2]][2])
  x0 <- as.numeric(parsed_header[[3]][2])
  y0 <- as.numeric(parsed_header[[4]][2])
  gridSize <- as.numeric(parsed_header[[5]][2])
  noVal <- as.numeric(parsed_header[[6]][2])
  
  x_label <- tolower(parsed_header[[3]][1])
  y_label <- tolower(parsed_header[[4]][1])
  
  if (grepl("corner", x_label) && grepl("corner", y_label)) {
    center_or_corner <- "corner"
  } else if (grepl("center", x_label) && grepl("center", y_label)) {
    center_or_corner <- "center"
  } else {
    warning("WARNING: unable to determine CenterOrCorner for origin of ascii file.")
    center_or_corner <- "NA"
  }
  

  data_matrix <- as.matrix(read.table(file_name, skip = 6, header = FALSE))
  dimnames(data_matrix) <- NULL 
  
  return(list(
    fileName = file_name,
    nX = nX,
    nY = nY,
    x0 = x0,
    y0 = y0,
    gridSize = gridSize,
    noVal = noVal,
    CenterOrCorner = center_or_corner,
    data = data_matrix
  ))
}

extract.at.points <- function(raster_obj, X, Y) {
  nPts <- length(X)
  if (nPts != length(Y)) {
    stop("extract.at.points: unequal x and y lengths")
  }
  
  if (raster_obj$CenterOrCorner == "NA") {
    stop("extract.at.points: file origin type is unknown.")
  }
  
  if (raster_obj$CenterOrCorner == "center") {
    iX <- floor((X - raster_obj$x0) / raster_obj$gridSize + 0.5) + 1
    iY <- raster_obj$nY - floor((Y - raster_obj$y0) / raster_obj$gridSize + 0.5)
    
  } else if (raster_obj$CenterOrCorner == "corner") {
    iX <- floor((X - raster_obj$x0) / raster_obj$gridSize) + 1
    iY <- raster_obj$nY - floor((Y - raster_obj$y0) / raster_obj$gridSize)
  }
  
  iX <- pmax(1, pmin(iX, raster_obj$nX))
  iY <- pmax(1, pmin(iY, raster_obj$nY))
  
  index_matrix <- cbind(iY, iX)
  ret <- raster_obj$data[index_matrix]
  
  return(ret)
}

## this makes the csv file if write = TRUE
make.qt.csv <- function(qt.grid.list, cdata, write = FALSE) {
  g2g_ids <- cdata$G2G.ID
  qt_dt <- data.table()
  for (id in g2g_ids){
    id_row <- cdata[G2G.ID %in% id, ]
    x <- id_row[,G2G.Easting]
    y <- id_row[,G2G.Northing]
    qt_vals <- lapply(qt.grid.list, function(qt){
      extract.at.points(qt, x, y)
    })
    qt_row <- data.table(G2G.ID = id_row$G2G.ID, Site= id_row$Site.No,	G2G_Easting =x ,	G2G_Northing = y,	qmed =qt_vals$qmed ,	q5 = qt_vals$q5,	q10=qt_vals$q10,	q25=qt_vals$q25,	q50=qt_vals$q50,	q75=qt_vals$q75,	q100=qt_vals$q100,	q200=qt_vals$q200,	q250=qt_vals$q250,	q1000=qt_vals$q1000)
    qt_dt <- rbind(qt_row, qt_dt)
  }
  if (write){
    write.csv(qt_dt, "../data/qt_g2g_sites_new.csv", row.names = FALSE)
  }
  return(qt_dt)
}


## Normalise every grid to common corner-origin
normalise.to.corner <- function(g) {
  if (g$CenterOrCorner == "center") {
    g$x0 <- g$x0 - g$gridSize / 2
    g$y0 <- g$y0 - g$gridSize / 2
    g$CenterOrCorner <- "corner"
    cat("  -> converted", basename(g$fileName), "from CENTER to CORNER origin\n")
  } else if (g$CenterOrCorner == "NA") {
    stop("Cannot normalise grid with unknown origin convention: ", g$fileName)
  }
  g
}


# sanitising check for making sure grids align
check.grid.alignment <- function(grid_list, tol = 1e-6) {
  ref <- grid_list[[1]]
  for (i in seq_along(grid_list)[-1]) {
    g <- grid_list[[i]]
    ok <- (g$nX == ref$nX) && (g$nY == ref$nY) &&
      (abs(g$x0 - ref$x0) < tol) && (abs(g$y0 - ref$y0) < tol) &&
      (abs(g$gridSize - ref$gridSize) < tol)
    if (!ok) {
      stop(sprintf(
        "Grid mismatch: '%s' does not align with reference '%s'.\n  ref: nX=%d nY=%d x0=%g y0=%g cell=%g\n  this: nX=%d nY=%d x0=%g y0=%g cell=%g",
        g$fileName, ref$fileName, ref$nX, ref$nY, ref$x0, ref$y0, ref$gridSize,
        g$nX, g$nY, g$x0, g$y0, g$gridSize
      ))
    }
  }
  invisible(TRUE)
}



ascii.to.rast <- function(g, crs = "EPSG:27700") {
  m <- g$data
  m[m == g$noVal] <- NA
  r <- rast(
    m,
    extent = ext(g$x0, g$x0 + g$nX * g$gridSize, g$y0, g$y0 + g$nY * g$gridSize),
    crs = crs
  )
  names(r) <- tools::file_path_sans_ext(basename(g$fileName))
  r
}
#


## extracting maxflow grids
load.maxflow.event <- function(file_path) {
  g <- normalise.to.corner(read.ascii(file_path)) ## load the maxflow grids by calling read.ascii
  ascii.to.rast(g)
}

build.exceedance.grid <- function(maxflow_rast, qt_rasts_ordered) {
  qt_rasts_ordered <- lapply(qt_rasts_ordered, function(r) resample(r, maxflow_rast, method = "near")) ## this aligns the qt 

  # a cell only counts as "on network" if it has a threshold value in AT LEAST ONE qt grid
  on_network <- !is.na(qt_rasts_ordered[[1]])
  for (r in qt_rasts_ordered[-1]) on_network <- on_network | !is.na(r)

  category <- rast(maxflow_rast)
  values(category) <- NA_integer_        # <- NA everywhere by default, not 0

  thresh_names <- names(qt_rasts_ordered)
  for (i in seq_along(qt_rasts_ordered)) {
    exceeded <- (maxflow_rast >= qt_rasts_ordered[[i]])
    category[exceeded] <- i
  }

  category[on_network & is.na(category)] <- 0L

  category <- mask(category, maxflow_rast)   # still respect the flow grid's own NODATA
  levels(category) <- data.frame(id = 0:length(thresh_names), category = c("None", thresh_names))
  category
}


# build.flow.dir <- function(maxflow_rast, flow_dir) {


#   flow_dir_ordered <- resample(flow_dir, maxflow_rast, method="near")
#   # a cell only counts as "on network" if it has a threshold value in AT LEAST ONE qt grid
#   # on_network <- !is.na(flow_dir_ordered)




#   for (r in flow_dir_ordered[-1]) on_network <- on_network | !is.na(r)

#   category <- rast(maxflow_rast)
#   values(category) <- NA_integer_        # <- NA everywhere by default, not 0

#   thresh_names <- names(flow_dir_ordered)
#   for (i in seq_along(flow_dir_ordered)) {
#     exceeded <- (maxflow_rast >= flow_dir_ordered[[i]])
#     category[exceeded] <- i
#   }

#   category[on_network & is.na(category)] <- 0L

#   category <- mask(category, maxflow_rast)   # still respect the flow grid's own NODATA
#   levels(category) <- data.frame(id = 0:length(thresh_names), category = c("None", thresh_names))
#   category
# }

extract.at.sites <- function(rast_obj, sites_df) {
  pts <- vect(sites_df, geom = c("easting", "northing"), crs = crs(rast_obj))
  vals <- extract(rast_obj, pts)
  cbind(sites_df, vals[, -1, drop = FALSE])
}


crop.mask.to.wales <- function(rast_obj, wales_sf) {
  wales_vect <- vect(wales_sf)
  wales_vect <- project(wales_vect, crs(rast_obj))
  r <- crop(rast_obj, wales_vect)
  r <- mask(r, wales_vect)
  r
}



########################################### 
#    code to create csv table below       #
###########################################

# qt_grid_paths <- mixedsort(sort(file.path("../data/qt_grids", list.files("../data/qt_grids")))) ## relative paths to `*_g2g_1_nffs.dat` 
# qt_val <- sub("_.*", "", mixedsort(sort(list.files("../data/qt_grids"))))
# qt_grid_list <- lapply(qt_grid_paths, read.ascii)
# names(qt_grid_list) <- qt_val ## name by qt

# qt_dt <- make.qt.csv(qt_grid_list, cdata = wales_cdata) ## can replace with whichever cdata, in this case it is cdata filtered by Region. == "Wales"
