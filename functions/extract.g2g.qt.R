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



########################################### 
#    code to create csv table below       #
###########################################

# qt_grid_paths <- mixedsort(sort(file.path("../data/qt_grids", list.files("../data/qt_grids")))) ## relative paths to `*_g2g_1_nffs.dat` 
# qt_val <- sub("_.*", "", mixedsort(sort(list.files("../data/qt_grids"))))
# qt_grid_list <- lapply(qt_grid_paths, read.ascii)
# names(qt_grid_list) <- qt_val ## name by qt

# qt_dt <- make.qt.csv(qt_grid_list, cdata = wales_cdata) ## can replace with whichever cdata, in this case it is cdata filtered by Region. == "Wales"
