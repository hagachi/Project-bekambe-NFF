# Initialization ---------------------------------------------------------
rm(list = ls())
gc()
root.dir <- rprojroot::find_root('Project-bekambe-NFF.Rproj')
script.dir <- paste0(root.dir, "/script/R")
source(file.path(script.dir, 'initialization.R'))

# MAIN ===================================================================
# cpu_num <- parallel::detectCores(logical = TRUE) # get an available number of cpu
cpu_num <- 8 # PC01 have 12 CPUs, but CPU を増やすと計算がエラーを起こしやすかった
cl <- makePSOCKcluster(cpu_num)
registerDoParallel(cl)
objects <- ls(envir=parent.frame())
t.start <- Sys.time()
foreach (scenario = scenario.name.list, # roop iterator
         .packages = package_list, # packages
         .export=objects # objects
         ) %dopar% {
  for (climate in climate.name.list) {
    scenario.dir <- paste0(root.dir, '/', scenario, "/", climate)
    print(scenario.dir)
    # 1. Compute LULC =============
    print("Start computeLULC.R")
    computeLULC()

    # 2. Compute HSI for blakiston's fish owl =============
    print("Start computeOwlHSI.R")
    computeOwlHSI()

    # 3. Compute HSI for blakiston's fish owl =============
    print("Start evaluate_owl_habitat.R")
    evaluate_owl_habitat()

    # 4. Compute HSI for kumataka =============
    print("Start computeKumatakaHSI.R")
    computeKumatakaHSI()

    # 5. Compute DSI and harmony index=============
    # print("Start compute_DSI_harmony.R")
    # compute_DSI_harmony()
    # 5. Compute DSI and harmony index=============
    print("Start compute_DSI_harmony.R")
    compute_DSI_harmony_julialang()
    
    # 6. Compute riparian zone forest biomass
    print("Start computeRiparianBiomass.R")
    computeRiparianBiomass()
  }
}
stopCluster(cl) # Stop clusters for multiprocessing
cat(paste('Elapsed time', round(difftime(Sys.time(), t.start, units = 'hours'), digits = 2), ' (hours)\n\n'))

# 8. Compute Energy ==========
source(file.path(script.dir, 'computeEnergy.R'))
