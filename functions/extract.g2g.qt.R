read.ascii <- function(file_name) {
  cat("*opening ascii: ", file_name, "\n")
  
  # 1. Read the first 6 lines safely
  # readLines handles the file connection and automatically closes it
  header_lines <- readLines(file_name, n = 6, warn = FALSE)
  
  # 2. Split each line by whitespace to separate the labels from the values
  parsed_header <- strsplit(trimws(header_lines), "\\s+")
  
  # 3. Extract the metadata 
  # (Note: R uses 1-based indexing, so [[1]][2] means line 1, item 2)
  nX <- as.integer(parsed_header[[1]][2])
  nY <- as.integer(parsed_header[[2]][2])
  x0 <- as.numeric(parsed_header[[3]][2])
  y0 <- as.numeric(parsed_header[[4]][2])
  gridSize <- as.numeric(parsed_header[[5]][2])
  noVal <- as.numeric(parsed_header[[6]][2])
  
  # 4. Check for center or corner origin
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
  
  # 5. Load the remaining grid data
  # read.table natively handles reading space-separated grids into a dataframe.
  # We skip the 6 header lines and immediately convert it to a numeric matrix.
  data_matrix <- as.matrix(read.table(file_name, skip = 6, header = FALSE))
  dimnames(data_matrix) <- NULL # Removes default column names like V1, V2, etc.
  
  # 6. Return everything as a bundled list
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
    stop("extract_at_points: unequal X and Y lengths")
  }
  
  if (raster_obj$CenterOrCorner == "NA") {
    stop("extract_at_points: cannot extract because file origin type is unknown (NA).")
  }
  
  if (raster_obj$CenterOrCorner == "center") {
    # Math for Center-aligned grids
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

test <- read.ascii("../data/for_cdata/accuk_nffs.asc")
test <- read.ascii("../data/for_cdata/q5_g2g_1_nffs.dat")

extraction <- extract.at.points(test, X = 307500, Y = 245500)
extraction
