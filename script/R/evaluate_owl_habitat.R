# title: evaluate Owl habitat
# auther: marimi
# date: 2019.11.22

evaluate_owl_habitat <- function() {
  # Setting ==================================================================
  # Import dataset
  setwd(paste0(data.dir, "/owl"))
  riparian_zone_name <- 'riparian_zone.tif'
  riparian_zone_ras <- raster(riparian_zone_name)
  riparian_zone_df <- as.data.frame(riparian_zone_ras)
  
  # MAIN ==================================================================
  t.start <- proc.time()
  # for (scenario in scenario.name.list) {
  #   for (climate in climate.name.list) {
  #     scenario.dir <- paste0(root.dir, '/', scenario, "/", climate)
  #     print(scenario.dir)
  for (tms in tms.list) {
    if (tms %% 10 == 0) print(tms)
    
    # get input raster
    setwd(paste0(scenario.dir, "/OutputMaps/biomass"))  
    total_biomass_name <- paste0("TotalBiomass-", tms, ".tif")    # biomass [g/m2]
    total_biomass_ras <- raster(total_biomass_name)
    
    # setwd(paste0(scenario.dir, "/OutputMaps/CohortStats/age-all-spp"))
    # age_max_name <- paste0("AGE-MAX-", tms, ".img")
    # age_max_ras <- raster(age_max_name)
    
    setwd(lulc.dir)
    dominantGrass.name <- paste0("dominantGrass-", scenario, "_", climate, "_", tms, ".tif")
    dominantGrass.ras <- raster(dominantGrass.name)
    isForest_ras <- dominantGrass.ras %in% c(1:7)
    
    forest_biomass_ras <- total_biomass_ras * isForest_ras
    forest_biomass_df <- data.frame(as.data.frame(forest_biomass_ras),
                                    riparian_zone_df) #,
                                    # as.data.frame(age_max_ras))
    
    colnames(forest_biomass_df) <- c('biomass', 'PageNumber') # , 'maxAge')
    
    riparian_forest_4km_biomass_df <- forest_biomass_df %>%
      filter(biomass > 0) %>%
      filter(PageNumber < 128) %>%
      group_by(PageNumber) %>%
      summarise(forest_ha = n(),
                forest_biomass = sum(biomass)) # ,
                # max_age = max(maxAge),
                # mean_age = mean(maxAge),
                # sd_age = sd(maxAge))
    
    # CSVで出力
    setwd(owl.eval.dir)
    out.name <- paste0("riparian_forest_4km_biomass_", scenario, "_", climate, "_", tms,".csv")
    write.csv(riparian_forest_4km_biomass_df, out.name)
  }
  #   }
  # }
  
  print("Total: ")
  print((proc.time() - t.start)/60)
  print("minutes")
  
  
}


# a <- forest_biomass_df%>%
#   filter(PageNumber < 128) %>% 
#   filter(biomass > 0) 
# hist(a$biomass)
# summary(a$biomass)
