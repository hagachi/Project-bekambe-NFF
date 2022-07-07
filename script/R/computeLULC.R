# Setting ==================================================================
# load function from script
source(paste0(script.dir, "/function_makeDominantSppMap.R"))

computeLULC <- function() {
  # MAIN ==================================================================
  t.start <- proc.time()
  # for (scenario in scenario.name.list) {
  #   for (climate in climate.name.list) {
  #     scenario.dir <- paste0(root.dir, '/', scenario, "/", climate)
  #     print(scenario)
  #     # Initialize progress bar
  #     pb <- txtProgressBar(min = 1,
  #                          max = length(tms.list),
  #                          style = 3)
  #     i_pb <- 1
  for(tms in tms.list){
    # Detect dominant species =================================
    dominant.ras <- MakeDominantSppMap(root.dir, scenario.dir, scenario, tms, spp.name.list) # SEE 'function_makeDominantSppMap.R'
    dominant.spp.ras <- dominant.ras[[1]]
    out.spp.name <- paste0(root.dir, "/Analysis/LULC_maps/dominantSpp-", scenario, "_", climate, "_", tms, ".tif")
    writeRaster(dominant.spp.ras, out.spp.name, overwrite = TRUE)
    dominant.grass.ras <- dominant.ras[[2]]
    out.grass.name <- paste0(root.dir, "/Analysis/LULC_maps/dominantGrass-", scenario, "_", climate, "_", tms, ".tif")
    writeRaster(dominant.grass.ras, out.grass.name, overwrite = TRUE)
    # Update the progress bar
    # setTxtProgressBar(pb, i_pb)
    # i_pb <- i_pb + 1
  }
  #   }
  # }
  
  print("Total: ")
  print((proc.time() - t.start)/60)
  print("minutes")
  
}
