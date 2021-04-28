prepInputsVRI <- function(VRIurl, dPath, rasterToMatch){
  VRIin <- prepInputs(url = VRIurl,
                      fun = "sf::st_read",
                      destinationPath = dPath)
  RIA_VRI <- st_transform(VRIin, crs = st_crs(rasterToMatch))
  gcIDRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "curve2")
  ageRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "PROJ_AGE_1")
  gcIDRaster[] <- as.integer(gcIDRaster[])
  ageRaster[] <- as.integer(ageRaster[])
  VRIraster <- raster::stack(gcIDRaster, ageRaster)
  return(VRIraster)
}

prepInputsVRIage <- function(VRIurl, dPath, rasterToMatch, targetFile, field = "PROJ_AGE_1"){
  sa <- as(extent(rasterToMatch), "SpatialPolygons")
  crs(sa) <- crs(rasterToMatch)
  VRIin <- prepInputs(url = VRIurl,
                      targetFile = targetFile,
                      archive = NA,
                      fun = NA,
                      destinationPath = dPath,
                      studyArea = sa
                      )
  vriAge2015 <- sf::st_read(paste0(dPath, "/", targetFile))
  RIA_VRI <- sf::st_transform(vriAge2015, st_crs(sa))
 # RIA_VRI <- st_transform(VRIin, crs = st_crs(rasterToMatch))
  ageRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "PROJ_AGE_1")
  ageRaster[] <- as.integer(ageRaster[])
  mrWdata <- !is.na(rasterToMatch)
  ageRaster[!mrWdata] <- NA
  return(ageRaster)
}

# Eliot's update April 23rd, 2021
# sa <- as(extent(masterRaster), "SpatialPolygons")
# crs(sa) <- crs(masterRaster)
# loadAge <- function(x, field = "PROJ_AGE_1") {
#   a <- sf::st_read(x)
#   a[, field]
# }
# a <- prepInputs(url =
#                   "https://pub.data.gov.bc.ca/datasets/02dba161-fdb7-48ae-a4bb-bd6ef017c36d/2015/VEG_COMP_LYR_L1_POLY_2015.gdb.zip",
#                 fun = quote(loadAge(x = targetFilePath,
#                                     field = "PROJ_AGE_1")),
#                 targetFile = "VEG_COMP_LYR_L1_POLY_2015.gdb.zip", archive = NA,
#                 studyArea = sa,
#                 useSAcrs = TRUE # transform it to CRS of the studyArea argument
# )
# # I needed to cast it to a MULTIPOLYGON ... and of course, you will need masterRaster
# aa <- fasterize::fasterize(sf::st_cast(a, to = "MULTIPOLYGON"), masterRaster, field = "PROJ_AGE_1")

## NEED TO
#I think I need to add studyArea = sa and useSAcrs = TRUE - don't you think?
# from E
# Oops... yes. You should (at least studyArea = sa). If you do the useSAcrs then
# you should be able to omit your st_transform line.
# currently in this branch of development (April27th, 2021)
#remotes::install_github("PredictiveEcology/reproducible@gdb_archiveNA")

