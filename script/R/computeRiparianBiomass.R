# compute riparian biomass
# buffer: 100m from river

computeRiparianBiomass <- function() {
  # Setting ==================================================================
  # Import dataset
  eco.ras <- raster(paste0(input.dir, "/ecoregions_bekambe_BaU_0_v1.0.tif")) # for crop
  setwd(data.dir)
  riparian_zone_name <- 'river_100m'
  riparian_zone_ras <- raster(riparian_zone_name)
  riparian_zone_ras[riparian_zone_ras==0]<- 1
  is.na(riparian_zone_ras) <- 0
  riparian_zone_ras <- crop(riparian_zone_ras, extent(eco.ras))
  
  for (tms in tms.list) {
    # Compute forest biomass =======================
    setwd(paste0(scenario.dir, "/OutputMaps/biomass"))
    forest_biomass_ras <- mosaic(raster(paste0("alnujapo-", tms, ".tif")), # g m^-2
                                 raster(paste0("betuplat-", tms, ".tif")),
                                 raster(paste0("fraxmand-", tms, ".tif")), 
                                 raster(paste0("quercris-", tms, ".tif")),
                                 raster(paste0("ulmudavi-", tms, ".tif")), 
                                 raster(paste0("abiesach-", tms, ".tif")),
                                 raster(paste0("larikaem-", tms, ".tif")),
                                 fun = sum)
    # extent(forest_biomass_ras)<- extent(riparian_zone_ras)
    
    writeRaster(forest_biomass_ras, paste0(riparian.dir, "/forest_biomass_", scenario, "_", climate, "_", tms, ".tif"))
    
    # compute riparian forest biomass
    extent(riparian_zone_ras) <- extent(forest_biomass_ras)
    riparian_forest_biomass_ras <- forest_biomass_ras*riparian_zone_ras
    writeRaster(riparian_forest_biomass_ras, paste0(riparian.dir, "/riparian_forest_biomass_", scenario, "_", climate, "_", tms, ".tif"))
    
    
  }
    
} 
    
    