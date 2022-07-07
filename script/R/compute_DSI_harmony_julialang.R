# title: compute DSI & harmony
# date: 2020.01.17
# auther: marimi, chihiro

# memo: prescriptionID 0-27 case

compute_DSI_harmony_julialang <- function() {
  # Julia setup ==============================================================
  library(JuliaCall)
  julia_pkg_import(julia_source(file.path(script.dir, 'function_DSI_harmony.jl')),
                   func_list = 'ComputeDSIMain')
  
  # Setting ==================================================================
  # Set parameters
  buf.num <- round(6000 * 0.5 * cell.len ^ -1) # number of cells from center of spatial unit to boundary
  # Import dataset
  setwd(data.dir)
  d.mat <- as.matrix(read.csv("dissimilarity_matrix.csv", header = TRUE, row.names = 1))
  # load function from script
  setwd(paste0(root.dir, "/script/R"))
  source("function_DSI_harmony.R")
  
  # MAIN ==================================================================
  for (tms in tms.list) {
    # 1. read raster data and convert it as matrix ======================================
    pasarea = as.numeric(str_sub(unlist(str_split(scenario, '_'))[2], start = 2, end = -1))
    lulc.out.list <- MakeLulcRaster(root.dir, input.dir, data.dir, lulc.dir,
                                    pasarea, scenario, climate, tms) # LULC for DSI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    lulc.ras <- lulc.out.list[[1]]
    out.lulc.name <- paste0(dsi.dir, "/dsi_lulc_", scenario, "_", climate, "_y", tms, ".tif")
    writeRaster(lulc.ras, out.lulc.name, overwrite = TRUE)
    
    ## make prescript map for 10 years
    if(tms > 9){
      prescript.stack <- lulc.out.list[[2]]
      pres.period.stack <- stack(prescript.stack[[tms-9]],
                                 prescript.stack[[tms-8]],
                                 prescript.stack[[tms-7]],
                                 prescript.stack[[tms-6]],
                                 prescript.stack[[tms-5]],
                                 prescript.stack[[tms-4]],
                                 prescript.stack[[tms-3]],
                                 prescript.stack[[tms-2]],
                                 prescript.stack[[tms-1]],
                                 prescript.stack[[tms]])
      # Make using place map
      # check whether every cells have been operated forest for 10 years or not
      operated.forest.ras <- calc(pres.period.stack %in% c(c(1:27,30:32)+1), sum) > 0      # !!!!!!forest management
      # check whether every cells is operated grassland or not
      operated.grass.ras <- pres.period.stack[[10]] %in% (28+1)           # !!!!!!!!! grass management
      
      # integrate these raster
      operated.ras <- operated.forest.ras + operated.grass.ras > 0
      # plot(operated.ras)
      out.use.name <- paste0(harmony.dir, "/usemap_", scenario, "_", climate, "_y", tms, ".tif")
      writeRaster(operated.ras, out.use.name, overwrite = TRUE)
      operated.mat <- as.matrix(operated.ras)
    } else {
      operated.mat <- matrix(NA, nrow(lulc.ras), ncol(lulc.ras))
    }
    
    # Compute DSI and Harmony index ======================================
    lulc.mat <- as.matrix(lulc.ras)
    results <- julia_call('ComputeDSIMain', lulc.mat, operated.mat, tms, d.mat, buf.num)

    # convert diver.mat and use.matfrom matrix to raster ===========================
    Q.ras <- raster(results[1])
    out.Q.name <- paste0(dsi.dir, "/Q_", scenario, "_", climate, "_y", tms, ".tif")
    writeRaster(Q.ras, out.Q.name, overwrite = TRUE)
    # save ntr.mat as raster
    ntr.ras <- raster(results[2])
    out.ntr.name <- paste0(dsi.dir, "/ntr_", scenario, "_", climate, "_y", tms, ".tif")
    writeRaster(ntr.ras, out.ntr.name, overwrite = TRUE)
    # save dsi.mat as raster
    dsi.ras <- raster(results[3])
    out.dsi.name <- paste0(dsi.dir, "/dsi_", scenario, "_", climate, "_y", tms, ".tif")
    writeRaster(dsi.ras, out.dsi.name, overwrite = TRUE)
    if(tms > 9){
      # save diver.mat as raster
      diver.art.ras <- raster(results[4])
      out.diver.art.name <- paste0(harmony.dir, "/diversity_withart_", scenario, "_", climate, "_y", tms, ".tif")
      writeRaster(diver.art.ras, out.diver.art.name, overwrite = TRUE)
      diver.ntr.ras <- raster(results[5])
      out.diver.ntr.name <- paste0(harmony.dir, "/diversity_ntronly_", scenario, "_", climate, "_y", tms, ".tif")
      writeRaster(diver.ntr.ras, out.diver.ntr.name, overwrite = TRUE)
      # save use.mat as raster
      use.6km.ras <- raster(results[6])
      out.use.6km.name <- paste0(harmony.dir, "/use_6km_", scenario, "_", climate, "_y", tms, ".tif")
      writeRaster(use.6km.ras, out.use.6km.name, overwrite = TRUE)
    }
  }
}



