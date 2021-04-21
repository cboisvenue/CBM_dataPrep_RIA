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
                      destinationPath = dPath)
  vriAge2015 <- sf::st_read(paste0(dPath, "/", targetFile))
  RIA_VRI <- sf::st_transform(vriAge2015, st_crs(sa))
 # RIA_VRI <- st_transform(VRIin, crs = st_crs(rasterToMatch))
  ageRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "PROJ_AGE_1")
  ageRaster[] <- as.integer(ageRaster[])
  return(ageRaster)
}

