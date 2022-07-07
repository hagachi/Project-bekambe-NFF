# evaluation indicators and above ground biomass data into one CSV file
# use after -> main.R

# Initialization ---------------------------------------------------------
root.dir <- rprojroot::find_root('Project-bekambe-NFF.Rproj')
script.dir <- paste0(root.dir, "/script/R")
source(file.path(script.dir, 'initialization.R'))


# Define function --------------------------------------------------------------
TimeSeriesConverter <- function(x) {
  scenario <- x[1]
  climate <- x[2]
  if (!file.exists(file.path(output.dir, 'ts-data', paste0('ts_', scenario, '_', climate, '.parquet')))) {
    scenario.dir <- file.path(root.dir, scenario, climate)
    ## Spacial mean of DSI ------------------------------------------------------
    dsi.stack <- terra::rast(file.path(dsi.dir, paste0('dsi_', scenario, '_', climate, '_y', tms.list, '.tif')))
    dsi.df <- data.frame(Time = tms.list,
                         val = terra::global(dsi.stack, fun = mean)$mean,
                         varname = 'dsi', scenario = scenario, climate = climate)
    rm(dsi.stack); gc()
    
    ## Spacial mean of Diversity -----------------------------------------------
    diver_withart.stack <- terra::rast(file.path(harmony.dir, paste0('diversity_withart_', scenario, '_', climate, '_y', tms.list.harmony, '.tif')))
    diver_withart.df <- data.frame(Time = tms.list.harmony,
                                   val = terra::global(diver_withart.stack, fun = mean)$mean,
                                   varname = 'diver_withart', scenario = scenario, climate = climate)
    diver_ntronly.stack <- terra::rast(file.path(harmony.dir, paste0('diversity_ntronly_', scenario, '_', climate, '_y', tms.list.harmony, '.tif')))
    diver_ntronly.df <- data.frame(Time = tms.list.harmony,
                                   val = terra::global(diver_ntronly.stack, fun = mean)$mean,
                                   varname = 'diver_ntronly', scenario = scenario, climate = climate)
    harmony.df <- dplyr::bind_rows(diver_withart.df, diver_ntronly.df)
    rm(diver_withart.stack); gc()
    
    ## Toal timber volumes and pasture -----------------------------------------
    # noManagementも木材収穫あり
    harvest.db <- readr::read_csv(file.path(scenario.dir, 'biomass-harvest-summary-log.csv'), col_types = readr::cols())
    if (scenario == "around_a0_s0.0_noManagement") {
      grass_harvest.df <- data.frame(Time = tms.list) %>%
        dplyr::mutate(val = 0, varname = 'grass_harvest', scenario = scenario, climate = climate)
    } else {
      # grass harvest
      grass_harvest.df <- harvest.db %>%
        dplyr::filter(Prescription == "Agriculture")%>%
        # tms.lista???a??a??a??ea??a?ca??e?N?cR?a?C-a???a??a\-a??fa??a?|a??a???a??NAa???a?aa??a1?La?Rbioma??fOa???a??a??
        dplyr::full_join(data.frame(tms = tms.list), by = c('Time' = 'tms')) %>%
        dplyr::filter(Time %in% tms.list)%>%
        dplyr::group_by(Time) %>%
        dplyr::summarise(val = sum(BiomassHarvestedMg_pastgras)) %>%  # biom (Mg)
        dplyr::ungroup() %>%
        dplyr::mutate(val = dplyr::if_else(is.na(val), 0, val),
                      varname = 'grass_harvest', scenario = scenario, climate = climate)
    }
    # timber harvest (woody biomass harvest without thinning and abandoned (for energy))
    timber_harvest.df <- harvest.db %>%
      dplyr::filter(Prescription != "Agriculture") %>%
      dplyr::filter(Prescription != "stop_and_solar") %>%
      dplyr::filter(Prescription != "thinning_conifer") %>%
      dplyr::filter(Prescription != "thinning_broad") %>%
      dplyr::filter(Prescription != "betuplat_ClearCutting_abandoned") %>%
      dplyr::filter(Prescription != "quercris_ClearCutting_abandoned") %>%
      dplyr::filter(Prescription != "abandonment_and_plant_broad") %>%
      dplyr::full_join(data.frame(tms = tms.list), by = c('Time' = 'tms')) %>%
      dplyr::filter(Time %in% tms.list) %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(sum(BiomassHarvestedMg_betuplat),
                                 sum(BiomassHarvestedMg_fraxmand),
                                 sum(BiomassHarvestedMg_quercris),
                                 sum(BiomassHarvestedMg_ulmudavi),
                                 sum(BiomassHarvestedMg_alnujapo),
                                 sum(BiomassHarvestedMg_larikaem),
                                 sum(BiomassHarvestedMg_abiesach))) %>%  # biom (Mg)
      dplyr::ungroup() %>%
      dplyr::mutate(val = dplyr::if_else(is.na(val), 0, val),
                    varname = 'timber_harvest', scenario = scenario, climate = climate)
    harvest.df <- dplyr::bind_rows(grass_harvest.df, timber_harvest.df)
    rm(harvest.db, grass_harvest.df, timber_harvest.df); gc()
    
    ## HSI, Vegetation naturalness(植生自然度), pastureArea ---------------------
    readcsvs <- function(fname) {
      tms <- rev(stringr::str_split(basename(fname), '_', simplify = T))[1]
      tms <- as.numeric(stringr::str_remove(tms, '.csv'))
      suppressMessages(data.table::fread(fname)) %>%
        dplyr::mutate(Time = tms)
    }
    owl.tmp.df <- file.path(owl.dir, paste0('owl_hsi_', scenario, '_', climate, '_', tms.list, '.csv')) %>%
      purrr::map(readcsvs) %>%
      purrr::reduce(dplyr::bind_rows)
    owl.df <- owl.tmp.df %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = mean(hsi, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'owl_hsi', scenario = scenario, climate = climate)
    owlCanoe.df <- owl.tmp.df %>%
      dplyr::filter(PageNumber %in% canoeViewPoints) %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = mean(hsi, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'owl_canoe', scenario = scenario, climate = climate)
    owl.forestBiomass.df <- file.path(owl.eval.dir, paste0('riparian_forest_4km_biomass_', scenario, '_', climate, '_', tms.list, '.csv')) %>%
      purrr::map(readcsvs) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(forest_biomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'owl_forestBiomass', scenario = scenario, climate = climate)
    kumataka.df <- file.path(kumataka.dir, paste0('kumataka_hsi_', scenario, '_', climate, '_', tms.list, '.csv')) %>%
      purrr::map(readcsvs) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = mean(hsi, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'kumataka_hsi', scenario = scenario, climate = climate)
    rm(owl.tmp.df); gc()
    # forest type
    forestType.stack <- terra::rast(file.path(owl.dir, paste0('forest_type_', scenario, '_', climate, '_', tms.list, '.tif')))
    forestType.df <- tibble::as_tibble(terra::freq(forestType.stack)) %>%
      dplyr::rename(Time = layer, forest_type = value, area_ha = count)
    # ripariam zone forest biomass 100m
    riparian.forest.biomass.stack <- terra::rast(file.path(riparian.dir, paste0('riparian_forest_biomass_', scenario, '_', climate, '_', tms.list, '.tif')))
    # Sum by layers
    # convert "g m-2" to "g" (*100*100) and "Gg-biomass"
    riparian.df <- data.frame(Time = tms.list,
                              val = terra::global(riparian.forest.biomass.stack, sum, na.rm = T)$sum * 10^4 * 10^-9,
                              varname = 'riparian_biomass_100m', scenario = scenario, climate = climate)
    hsi.df <- dplyr::bind_rows(owl.df, owlCanoe.df, owl.forestBiomass.df, kumataka.df, riparian.df)
    rm(owl.df, owlCanoe.df, owl.forestBiomass.df, kumataka.df, riparian.df); gc()
    vegeNatu.df <- forestType.df %>%
      dplyr::filter(forest_type <= 5, forest_type > 0) %>%   # select natural forest from 植生自然度
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(area_ha, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'vegeNatu_area', scenario = scenario, climate = climate)
    pastureArea.df <- forestType.df %>%
      dplyr::filter(forest_type == 10) %>%   # select pastureland
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(area_ha, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'pasture_area', scenario = scenario, climate = climate)
    rm(forestType.df); gc()
    
    ## NEP ----------------------------------------------------------------------
    necn.df <- readr::read_csv(file.path(scenario.dir, 'NECN-succession-monthly-log.csv'), col_types = readr::cols()) %>%
      dplyr::select(Time, NumSites, avgNEE) %>% # gC m^-2
      dplyr::mutate(sumNEP = NumSites * -avgNEE * 10^4 * 10^-12) %>% # ha * gCm^-2 * 10^4 *10^-12 = TgC
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(sumNEP), .groups = 'drop') %>% # TgC/yr
      dplyr::mutate(varname = 'yrNEP_TgC', scenario = scenario, climate = climate)
    # Flux (TgC/yr)
    nep.df <- necn.df
    # Cumulative NEP
    nep.cumsum.df <- necn.df %>%
      dplyr::group_by(scenario) %>%
      dplyr::mutate(val = cumsum(val)) %>% # TgC
      dplyr::ungroup() %>%
      dplyr::mutate(varname = 'cumsumNEP_TgC', scenario = scenario, climate = climate)
    rm(necn.df); gc()
    
    ## Biomass ------------------------------------------------------------------
    # total
    totalBiomass.all.df <- readr::read_csv(file.path(analysis.dir, paste0('Biomass_total/totalBiomassDB_', scenario, '_', climate, '.csv')), col_types = readr::cols())
    colnames(totalBiomass.all.df) <- c("Time", "spp_name", "scenario", "climate", "totalBiomass")
    totalBiomass.df <- totalBiomass.all.df %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>% # Gg-biomass from visualizeBiomass.R
      dplyr::mutate(varname = 'totalBiomass', scenario = scenario, climate = climate)
    # natural forest
    naturalBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name != "larikaem") %>%
      dplyr::filter(spp_name != "pastgras") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'naturalBiomass', scenario = scenario, climate = climate)
    # conifer forest
    coniferBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name == "larikaem" | spp_name == "abiesach") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'coniferBiomass', scenario = scenario, climate = climate)
    # broad leaf forest
    broadBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name != "larikaem") %>%
      dplyr::filter(spp_name != "abiesach") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'broadlBiomass', scenario = scenario, climate = climate)
    # larikaem
    larikaemBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name == "larikaem") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'larikaemBiomass', scenario = scenario, climate = climate)
    # abiesach
    abiesachBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name == "abiesach") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'abiesachBiomass', scenario = scenario, climate = climate)
    # betuplat
    betuplatBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name == "betuplat") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'betuplatBiomass', scenario = scenario, climate = climate)
    # quercris
    quercrisBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name == "quercris") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'quercrisBiomass', scenario = scenario, climate = climate)
    # ulmudavi
    ulmudaviBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name == "ulmudavi") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'ulmudaviBiomass', scenario = scenario, climate = climate)
    # alnujapo
    alnujapoBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name == "alnujapo") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'alnujapoBiomass', scenario = scenario, climate = climate)
    # fraxmand
    fraxmandBiomass.df <- totalBiomass.all.df %>%
      dplyr::filter(spp_name == "fraxmand") %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(val = sum(totalBiomass, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(varname = 'fraxmandBiomass', scenario = scenario, climate = climate)
    rm(totalBiomass.all.df); gc()
    
    ## Viewshed analysis -------------------------------------------------------
    # Read LULC dataset used in diversity assessment
    lulc.stack <- terra::rast(file.path(dsi.dir, paste0('dsi_lulc_', scenario, '_', climate, '_y', tms.list, '.tif')))
    eco.ras <- terra::rast(file.path(input.dir, 'ecoregions_bekambe_BaU_0_v1.0.tif'))
    terra::ext(lulc.stack) <- terra::ext(eco.ras)
    culture.mask <- terra::rast(file.path(data.dir, 'viewshed', 'mask_culture.tif'))
    tourism.mask <- terra::rast(file.path(data.dir, 'viewshed', 'mask_tourism.tif'))
    # All
    lulc.df <- terra::freq(lulc.stack) %>% 
      tibble::as_tibble() %>%
      dplyr::rename(Time = layer, class = value, area_ha = count) %>%
      dplyr::mutate(varname = 'totallulc', scenario = scenario, climate = climate)
    # 生活環境
    # 居住地tif OR 主要な道路tif
    lulc.culture.stack <- terra::mask(x = lulc.stack, mask = culture.mask)
    viewshed_culture.df <- terra::freq(lulc.culture.stack) %>%
      tibble::as_tibble() %>%
      dplyr::rename(Time = layer, class = value, area_ha = count) %>%
      dplyr::mutate(varname = 'viewshedCulture', scenario = scenario, climate = climate)
    rm(lulc.culture.stack)
    # 観光資源
    ## カヌーtif OR 都道府県指定文化財tif OR 地域資源tif OR 自然公園地域shape
    lulc.tourism.stack <- terra::mask(x = lulc.stack, mask = tourism.mask)
    viewshed_tourism.df <- terra::freq(lulc.tourism.stack) %>%
      tibble::as_tibble() %>%
      dplyr::rename(Time = layer, class = value, area_ha = count) %>%
      dplyr::mutate(varname = 'viewshedTourism', scenario = scenario, climate = climate)
    rm(lulc.tourism.stack)
    
    # Merge them
    dplyr::bind_rows(dsi.df, hsi.df, harmony.df, harvest.df, vegeNatu.df, pastureArea.df,
                     nep.df, nep.cumsum.df, totalBiomass.df, naturalBiomass.df, coniferBiomass.df,
                     broadBiomass.df, larikaemBiomass.df, abiesachBiomass.df, betuplatBiomass.df,
                     quercrisBiomass.df, ulmudaviBiomass.df, alnujapoBiomass.df, fraxmandBiomass.df,
                     viewshed_culture.df, viewshed_tourism.df, lulc.df) %>% 
      arrow::write_parquet(file.path(output.dir, 'ts-data', paste0('ts_', scenario, '_', climate, '.parquet')))
    rm(dsi.df, hsi.df, harmony.df, harvest.df, vegeNatu.df, pastureArea.df,
       nep.df, nep.cumsum.df, totalBiomass.df, naturalBiomass.df, coniferBiomass.df,
       broadBiomass.df, larikaemBiomass.df, abiesachBiomass.df, betuplatBiomass.df,
       quercrisBiomass.df, ulmudaviBiomass.df, alnujapoBiomass.df, fraxmandBiomass.df,
       viewshed_culture.df, viewshed_tourism.df)
    gc()
  }
}

CalcSimilarity <- function(df) {
  view.start <- dplyr::filter(df, Time == 1) %>% 
    dplyr::select(-Time)
  view.tms <- dplyr::select(df, -Time)
  view.bray.sim <- 1 - vegan::vegdist(view.tms, method = 'bray')
  view.bray.sim <- as.vector(as.matrix(view.bray.sim)[1, ])
  return(data.frame(Time = df$Time, val = view.bray.sim))
}

CalcSimpson <- function(df) {
  xs <- df$val
  pi <- xs / sum(xs)
  return(1 - sum(pi^2))
}


# Preprocessing ----------------------------------------------------------------
## Identify canoe viewable PageNumber ------------------------------------------
canoe.ras <- raster(file.path(data.dir, "viewshed_canoe.tif"))
lonlat.df <- read.csv(file.path(data.dir, "owl/bekanbe_4km_lonlat.txt"))
r <- raster(ncols = length(unique(lonlat.df$lon)), nrow = length(unique(lonlat.df$lat)),
            xmn = min(lonlat.df$lon), xmx = max(lonlat.df$lon), ymn = min(lonlat.df$lat), ymx = max(lonlat.df$lat))
pagenumber.ras <- rasterize(as.matrix(lonlat.df[,4:5]), r)
pagenumber.ras <- resample(pagenumber.ras, canoe.ras, method = "ngb")
canoeViewPoints <- unique(pagenumber.ras * (canoe.ras>0))
rasterVis::levelplot(pagenumber.ras * (canoe.ras>0), margin = F)

## Load viewshed data ----------------------------------------------------------
### Read dataset ----------
eco.ras <- terra::rast(file.path(input.dir, 'ecoregions_bekambe_BaU_0_v1.0.tif'))
if (!file.exists(file.path(data.dir, 'viewshed', 'viewshed_canoe_resampled.tif'))) {
  ### Read files
  view.canoe.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_canoe.tif'))
  view.assets.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_localassets.tif'))
  view.culture.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_prefculture.tif'))
  view.pop.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_pop.tif'))
  view.road.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_road.tif'))
  ## raster projection
  view.canoe.ras <- terra::project(view.canoe.ras, crs(eco.ras), method = 'near')
  view.assets.ras <- terra::project(view.assets.ras, crs(eco.ras), method = 'near')
  view.culture.ras <- terra::project(view.culture.ras, crs(eco.ras), method = 'near')
  view.pop.ras <- terra::project(view.pop.ras, crs(eco.ras), method = 'near')
  view.road.ras <- terra::project(view.road.ras, crs(eco.ras), method = 'near')
  ## Resampling
  view.canoe.ras <- terra::resample(view.canoe.ras, eco.ras, method = 'near')
  view.assets.ras <- terra::resample(view.assets.ras, eco.ras, method = 'near')
  view.culture.ras <- terra::resample(view.culture.ras, eco.ras, method = 'near')
  view.pop.ras <- terra::resample(view.pop.ras, eco.ras, method = 'near')
  view.road.ras <- terra::resample(view.road.ras, eco.ras, method = 'near')
  ## natural park shapefile
  naturalpark.sp <- terra::vect(file.path(data.dir, 'viewshed', 'naturalpark_cliped.shp'))
  naturalpark.sp <- terra::project(naturalpark.sp, crs(eco.ras))
  view.park.ras <- terra::rasterize(naturalpark.sp, eco.ras, field = "Shape_Leng") > 0
  view.park.ras[is.na(view.park.ras)] <- 0
  # Compute masks
  culture.mask <- (view.pop.ras > 0 | view.road.ras > 0)
  culture.mask[culture.mask == 0] <- NA
  tourism.mask <- (view.canoe.ras > 0 | view.culture.ras > 0 | view.assets.ras > 0 | view.park.ras > 0)
  tourism.mask[tourism.mask == 0] <- NA
  ## Save resampled dataset
  terra::writeRaster(view.canoe.ras, file.path(data.dir, 'viewshed', 'viewshed_canoe_resampled.tif'), overwrite = T)
  terra::writeRaster(view.assets.ras, file.path(data.dir, 'viewshed', 'viewshed_localassets_resampled.tif'), overwrite = T)
  terra::writeRaster(view.culture.ras, file.path(data.dir, 'viewshed', 'viewshed_prefculture_resampled.tif'), overwrite = T)
  terra::writeRaster(view.pop.ras, file.path(data.dir, 'viewshed', 'viewshed_pop_resampled.tif'), overwrite = T)
  terra::writeRaster(view.road.ras, file.path(data.dir, 'viewshed', 'viewshed_road_resampled.tif'), overwrite = T)
  terra::writeRaster(view.park.ras, file.path(data.dir, 'viewshed', 'viewshed_naturalpark_resampled.tif'), overwrite = T)
  terra::writeRaster(culture.mask, file.path(data.dir, 'viewshed', 'mask_culture.tif'), overwrite = T)
  terra::writeRaster(tourism.mask, file.path(data.dir, 'viewshed', 'mask_tourism.tif'), overwrite = T)
} else {
  view.canoe.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_canoe_resampled.tif'))
  view.assets.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_localassets_resampled.tif'))
  view.culture.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_prefculture_resampled.tif'))
  view.pop.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_pop_resampled.tif'))
  view.road.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_road_resampled.tif'))
  view.park.ras <- terra::rast(file.path(data.dir, 'viewshed', 'viewshed_naturalpark_resampled.tif'))
  culture.mask <- terra::rast(file.path(data.dir, 'viewshed', 'mask_culture.tif'))
  tourism.mask <- terra::rast(file.path(data.dir, 'viewshed', 'mask_tourism.tif'))
}

### Visualize -------------
plt1 <- levelplot(view.canoe.ras > 0, margin = F, main = 'viewshed from canoe')
plt2 <- levelplot(view.assets.ras > 0, margin = F, main = 'viewshed from assets')
plt3 <- levelplot(view.culture.ras > 0, margin = F, main = 'viewshed from cultural assets')
plt4 <- levelplot(view.pop.ras > 0, margin = F, main = 'viewshed from home')
plt5 <- levelplot(view.road.ras > 0, margin = F, main = 'viewshed from road')
plt6 <- levelplot(view.park.ras > 0, margin = F, main = 'viewshed within park')
plt7 <- levelplot(culture.mask, margin = F, main = 'Cultural mask')
plt8 <- levelplot(tourism.mask, margin = F, main = 'Tourism mask')
pdf(file.path(output.dir, '001_viewshed.pdf'), width = 16, height = 16)
gridExtra::grid.arrange(plt1, plt2, plt3, plt4, plt5, plt6, plt7, plt8, ncol = 3)
dev.off()


# MAIN -------------------------------------------------------------------------
## Compute differences between 2016-2100 for evaluation indicators -------------
case_combinations <- expand.grid(scenario.name.list, climate.name.list)
cpu_num <- 8
library(pbapply)
cl <- makeCluster(cpu_num, type = 'FORK')
clusterExport(cl, c('%>%', basic.vars, 'canoeViewPoints'))
cat('\n Started parallel process...\n')
res <- pbapply(case_combinations, TimeSeriesConverter, MARGIN = 1, cl = cl) 
cat('\n Finished parallel process...\n')
stopCluster(cl)

## Merge ts data -----------------------
## Ecological ts ----
eco.fnames <- list.files(path = file.path(output.dir, 'ts-data'), pattern = '.parquet', full.names = TRUE)
eco.fnames <- eco.fnames[str_detect(eco.fnames, 'ts_')]
eco.df <- map(eco.fnames, arrow::read_parquet) %>% 
  reduce(bind_rows)
## Compute similarity of viewshed --------
view.df <- eco.df %>% 
  dplyr::filter(varname %in% c('viewshedCulture', 'viewshedTourism')) %>% 
  dplyr::select(-val) %>% 
  tidyr::pivot_wider(names_from = class, values_from = area_ha)
naniar::miss_var_summary(view.df)
view.df[is.na(view.df)] <- 0
naniar::miss_var_summary(view.df) # OK
view.sim.df <- view.df %>% 
  dplyr::select(-`0`) %>% # remove outside the region
  group_nest(varname, scenario, climate) %>% 
  mutate(simdata = map(data, CalcSimilarity)) %>% # compute similarity
  select(-data) %>% unnest(simdata) %>% 
  mutate(varname = case_when(varname == 'viewshedCulture' ~ 'viewsimCulture',
                             varname == 'viewshedTourism' ~ 'viewsimTourism'))
naniar::miss_var_summary(view.sim.df)
## Compute natural ness ----------
prop.nature.df <- view.df %>% 
  mutate(val = 1 - (`801`) / (`101` + `102` + `103` + 
                                `202` + `203` + `204` + 
                                `301` + `302` + `303` + `305` + # 304は茶畑なので除外
                                `401` + `402` + `701` + `801`)) %>% 
  dplyr::select(Time, varname, scenario, climate, val) %>% 
  mutate(varname = case_when(varname == 'viewshedCulture' ~ 'propnatCulture',
                             varname == 'viewshedTourism' ~ 'propnatTourism'))
naniar::miss_var_summary(prop.nature.df)
## Compute Simpson's diversity index -----------
sdi.df <- eco.df %>% 
  dplyr::select(-class, -area_ha) %>% 
  dplyr::filter(str_detect(varname, 'Biomass') & 
                  !(varname %in% c('totalBiomass', 'naturalBiomass', 'coniferBiomass', 'broadlBiomass', 'owl_forestBiomass'))) %>% 
  dplyr::group_nest(Time, scenario, climate) %>% 
  dplyr::mutate(val = map(data, CalcSimpson)) %>% 
  dplyr::select(-data) %>% tidyr::unnest(val) %>% 
  dplyr::mutate(varname = 'sdi_biom')
head(sdi.df)
## total LULC ---------------
lulc.df <- eco.df %>% 
  dplyr::filter(varname == 'totallulc') %>% 
  dplyr::select(-val) %>% 
  tidyr::pivot_wider(names_from = class, values_from = area_ha) %>% 
  dplyr::mutate(val = 1 - (`801`) / (`101` + `102` + `103` + 
                                       `202` + `203` + `204` + 
                                       `301` + `302` + `303` + `305` + # 304は茶畑なので除外
                                       `401` + `402` + `701` + `801`)) %>% 
  dplyr::select(Time, varname, scenario, climate, val) %>% 
  dplyr::mutate(varname = 'naturalness')
naniar::miss_var_summary(lulc.df)

## Merge columns -----
naniar::miss_var_summary(eco.df)
eco.mod.df <- eco.df %>% 
  mutate(val = if_else(is.na(val), area_ha, val)) %>% 
  select(-area_ha) %>% 
  filter(!(str_detect(varname, 'viewshed') & class == 0))
naniar::miss_var_summary(eco.mod.df) # OK
# Energy ts
energy.df <- readr::read_csv(file.path(energy.dir, 'total_energy.csv'), col_types = cols()) %>%
  dplyr::select(Time, scenario, climate, total_energy) %>%
  dplyr::mutate(varname = 'energy') %>%
  rename(val = total_energy)
# Merge
data <- bind_rows(eco.mod.df, view.sim.df, energy.df, prop.nature.df, sdi.df, lulc.df) %>% 
  tidyr::separate(scenario, into = c('NFF', 'a', 's', 'ispasplant', 'cut', 'rotation', 'plantspp'), sep = '_')
# Fill NA on cut, rotation, and plantspp with ""
naniar::miss_var_summary(data)
data[is.na(data)] <- ""
naniar::miss_var_summary(data) # OK
arrow::write_parquet(data, file.path(output.dir, 'analyzeTimeSeries.parquet'))
# next step -> plot_indicators_separate_climate.R

