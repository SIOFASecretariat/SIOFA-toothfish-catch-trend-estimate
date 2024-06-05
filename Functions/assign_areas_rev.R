


assign_areas_rev <- function (Input, Polys, AreaNameFormat = "GAR_Long_Label", Buffer = 0, 
          NamesIn = NULL, NamesOut = NULL) 
{
  Input = as.data.frame(Input)
  if (is.null(NamesIn) == FALSE) {
    if (length(NamesIn) != 2) {
      stop("'NamesIn' should be a character vector of length 2")
    }
    if (any(NamesIn %in% colnames(Input) == FALSE)) {
      stop("'NamesIn' do not match column names in 'Input'")
    }
  }
  if (is.null(NamesOut) == TRUE) {
    NamesOut = Polys
  }
  if (any(NamesOut %in% colnames(Input) == TRUE)) {
    stop("'NamesOut' matches column names in 'Input', please use different names")
  }
  if (length(Buffer) == 1 & length(Polys) > 1) {
    Buffer = rep(Buffer, length(Polys))
  }
  if (length(AreaNameFormat) == 1 & length(Polys) > 1) {
    AreaNameFormat = rep(AreaNameFormat, length(Polys))
  }
  if (is.null(NamesIn) == TRUE) {
    Locs = Input[, c(2, 1)]
  }
  else {
    Locs = Input[, c(NamesIn[c(2, 1)])]
  }
 
   Missing = which(is.na(Locs[, 1]) == TRUE | is.na(Locs[, 2]) == 
                    TRUE)
  if (length(Missing) > 1) {
    warning(paste0(length(Missing), " records are missing location and will not be assigned to any area\n"))
  }
  if (length(Missing) == 1) {
    warning("One record is missing location and will not be assigned to any area\n")
  }
  Impossible = which(Locs[, 1] > 180 | Locs[, 1] < (-180) | 
                       Locs[, 2] > 90 | Locs[, 2] < (-90))
  if (length(Impossible) > 1) {
    warning(paste0(length(Impossible), " records are not on Earth and will not be assigned to any area\n"))
    Locs[Impossible, ] = NA
  }
  if (length(Impossible) == 1) {
    warning("One record is not on Earth and will not be assigned to any area\n")
    Locs[Impossible, ] = NA
  }
  Locu = unique(Locs)
  Locu = Locu[is.na(Locu[, 1]) == FALSE & is.na(Locu[, 2]) == 
                FALSE, ]
  SPls = st_as_sf(x = Locu, coords = c(1, 2), crs = 4326, remove = FALSE)
 # SPls = st_transform(x = SPls, crs = 6932)
  Assigned_Areas = data.frame(matrix(NA, nrow = dim(Locu)[1], 
                                     ncol = length(Polys), dimnames = list(NULL, NamesOut)))
  Assigned_Areas$Lat = Locu[, 2]
  Assigned_Areas$Lon = Locu[, 1]
  
  for (i in seq(1, length(Polys))) {
    tmpArea = get(Polys[i])
    if (Buffer[i] > 0) {
      tmpArea = st_buffer(tmpArea, dist = Buffer[i] * 1852)
    }
    match = sapply(st_intersects(SPls, tmpArea), function(z) if (length(z) == 
                                                                 0) 
      NA_integer_
      else z[1])
    tmpArea = st_drop_geometry(tmpArea)
    if (ncol(tmpArea) == 1) {
      tmpArea$xyz123 = NA
    }
    match = tmpArea[match, ]
    Assigned_Areas[, NamesOut[i]] = as.character(match[, 
                                                       AreaNameFormat[i]])
  }
  
  join_cols = c("Lat", "Lon")
  if (is.null(NamesIn) == TRUE) {
    names(join_cols) = colnames(Input)[c(1, 2)]
  }
  else {
    names(join_cols) = NamesIn
  }
  Input = dplyr::left_join(Input, Assigned_Areas, by = join_cols)
  Input = as.data.frame(Input)
  return(Input)
}