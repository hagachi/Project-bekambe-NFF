#' ---
#' title: Calculate HSI for Blackiston's fish owl
#' date: 2019.02.15
#' author: Chihiro Haga
#' 
#' memo: prescriptionID 0-27 case
#' ---

computeOwlHSI <- function() {
  # load functions from script
  source(paste0(script.dir, '/function_owl_hsi.R'))
  
  # Initialize objects
  owl.data.dir <- paste0(data.dir, '/owl')
  
  # read static data
  setwd(owl.data.dir)
  # natural forest distribution map inside the watershed
  vegetation_name <- 'natural_forest.tif'
  vegetation_ras <- raster(vegetation_name)
  
  # natural forest distribution map outside the watershed
  outside_natural_forest_name <- 'natural_riparian_forest_area_pagenum.txt'
  outside_natural_forest_df <- read_csv(outside_natural_forest_name) %>%
    rename(outside_area_ha = SUM_area_h) %>%
    select(PageNumber, outside_area_ha)
  
  # riparian zone distribution map inside the watershed
  riparian_zone_name <- 'riparian_zone.tif'
  riparian_zone_ras <- raster(riparian_zone_name)
  
  # stream length for each 4km grids
  stream_length_name <- 'stream_length_pagenumber.txt'
  stream_length_df <- read_csv(stream_length_name) %>%
    select(PageNumber, SUM_length)%>%
    rename(length_m = SUM_length)
  
  lonlat_4km_name <- 'bekanbe_4km_lonlat.txt'
  lonlat_4km_df <- read_csv(lonlat_4km_name)
  
  # merge stream length and natural forest
  hsi_variable_df <- stream_length_df %>%
    full_join(outside_natural_forest_df, by = 'PageNumber')
  
  # stream line shape
  stream_name <- 'bekanbe_river.shp'
  stream_sf <- sf::read_sf(stream_name) %>%
    st_transform(crs = 4612)
  
  # Identify abiesach natural forest
  setwd(paste0(root.dir, "/data/owl"))
  initial_planted <- raster("todo_kara_planted.tif")
  initial_planted[is.na(initial_planted)] <- 0
  
  
  
  hsi_dfs <- list()
  hsi_iter <- 1
  t.start <- proc.time()
  
  # for (scenario in scenario.name.list) {
  #   for (climate in climate.name.list) {
  #     scenario.dir <- paste0(root.dir, '/', scenario, "/", climate)
  #     print(scenario.dir)
  #     
  #     # Initialize progress bar
  #     pb <- txtProgressBar(min = 1,
  #                          max = length(tms.list),
  #                          style = 3)
  #     i_pb <- 1
  for (tms in tms.list) {
    
    # Read raster data as data stack
    agb_dir <- paste0(root.dir, "/", scenario, "/", climate, "/OutputMaps/biomass")
    setwd(agb_dir)
    agb_names <- paste0(spp.name.list, '-', tms, '.tif')
    agb_stack <- stack(agb_names)
    names(agb_stack) <- spp.name.list
    
    # Read support information
    lulc_dir <-paste0(root.dir, "/Analysis/LULC_maps")
    setwd(lulc_dir)
    lulc_name <- paste0('dominantGrass-', scenario, '_', climate, '_', tms, '.tif')
    lulc_ras <- raster(lulc_name)
    # isActivePasture <- lulc_ras == 8
    # isPotentialWetland <- lulc_ras == 31
    # isSolarPowerPlant <- lulc_ras == 21
    
    # identify artificial forest
    prescript_dir <- paste0(root.dir, "/", scenario, "/", climate, "/OutputMaps/harvest")
    setwd(prescript_dir)
    prescripts_name <- paste0("prescripts-", 1:tms, ".img")
    prescripts_stack <- raster::stack(prescripts_name)
    if (tms == 1) {
      is_artificial_forest <- prescripts_stack %in% c(c(5,6,8,10:12)+1) # !!!!!!!!clearcutting
      is_natural_forest <- prescripts_stack %in% c(c(1:4,13:27)+1) # !!!!!!!!!!!!!!!!selective cutting(as secondary forest)
    } else {
      is_artificial_forest <- identify_artificial_forest(prescripts_stack) # ID7:12を1に変換して、足し合わせ、1より大きいところを探す# clearcutting
      is_natural_forest <- identify_natural_forest(prescripts_stack)  # selective cutting(as secondary forest)
    }
    is_artificial_forest[is.na(is_artificial_forest)] <- 0  
    is_natural_forest[is.na(is_natural_forest)] <- 0 
    
    extent(vegetation_ras) <- extent(agb_stack)
    extent(is_artificial_forest) <- extent(agb_stack)
    extent(is_natural_forest) <- extent(agb_stack)
    extent(initial_planted) <- extent(agb_stack)
    
    dat_stack <- stack(agb_stack,
                       lulc_ras,
                       # isActivePasture,
                       # isPotentialWetland,
                       # isSolarPowerPlant,
                       is_artificial_forest,
                       is_natural_forest,
                       initial_planted)
    
    names(dat_stack) <- c(spp.name.list, 
                          'lulc_ras',
                          # 'isActivePasture',
                          # 'isPotentialWetland',
                          # 'isSolarPowerPlant',
                          "is_artificial", 
                          "is_natural",
                          "is_artificial_initial")
    forest_type_ras <- calc(dat_stack, compute_forest_type)
    
    # Fill ZERO sites with vegetationmap
    fill_stack <- stack(forest_type_ras, vegetation_ras)
    names(fill_stack) <- c('landis', 'initial')
    forest_type_ras <- calc(fill_stack, fill_zero_forest_type)
    
    # Save the forest type map
    setwd(owl.dir)
    forest_type_name <- paste0('forest_type_', scenario, '_', climate, "_", tms, '.tif')
    writeRaster(forest_type_ras, forest_type_name, overwrite = TRUE)
    
    
    # Compute natural forest area within riparian zone for each spatial units
    # 1. トドマツ－ミズナラ群落
    # 2. トドマツ群落（誘導林）
    # 3. ハルニレ群落
    # 4. ハンノキ－ヤチダモ群集
    # 5. ハンノキ群落（IV）
    # 6. シラカンバ－ミズナラ群落
    # 7. ササ群落（V）
    # 8. トドマツ植林
    # 9. カラマツ植林
    # 10. 牧草地
    forest_riparian_df <- data.frame(as.data.frame(forest_type_ras),
                                     as.data.frame(riparian_zone_ras))
    colnames(forest_riparian_df) <- c('type', 'PageNumber')
    inside_watershed_natural_riparian_df <- forest_riparian_df %>%
      filter(type <= 5, type > 0) %>%   # select natural forest from 植生自然度
      filter(PageNumber < 128) %>%    # only riparian zone
      group_by(PageNumber) %>%
      summarise(inside_area_ha = n())
    
    
    # Compute HSI for Blakiston's fish owl
    hsi_df <- hsi_variable_df %>%
      full_join(inside_watershed_natural_riparian_df, by = 'PageNumber')
    
    hsi_df[is.na(hsi_df)] <- 0 # replace NA to ZERO
    
    hsi_df <- hsi_df %>%
      mutate(scenario = scenario,
             climate = climate,
             year = tms + 2015,
             area_integ_ha = (outside_area_ha + inside_area_ha) , # integrate and convert unit
             length_m = length_m, # convert unit
             hsi = 1/(1 + exp(-(-23.36 
                                + 9.32 * 10^(-3) * area_integ_ha 
                                + 5.026 * log(length_m + 1) 
                                - 0.326 * log(length_m + 1)^2))))
    
    setwd(owl.dir)
    write.csv(hsi_df, paste0("owl_hsi_", scenario, "_", climate, "_", tms, ".csv"))
    # change to save without PageNumber from front in Ecol Evol
    # write.csv(hsi_lonlat_df, paste0("owl_hsi_", scenario, "_", climate, "_", tms, ".csv"))
    
    # setTxtProgressBar(pb, i_pb) 
    # i_pb <- i_pb + 1
  }
  #   }
  # }
  
  print("Total: ")
  print((proc.time() - t.start)/60)
  print("minutes")
  
}
