# Install packages
library(rgdal)
library(raster)
library(sf)
library(terra)
# library(tcltk)
library(tidyverse)
library(readr)
library(arrow)
library(viridis)
library(rasterVis)
library(ggthemes)
library(stringr)
library(doParallel) # for parallel computing
library(parallel)
library(patchwork)
library(plotly)


# set variables +++++++++++++++++++++++++++++++++++++++++++++++++
root.dir <- rprojroot::find_root('210113_bekambe_NFF_v1.5.Rproj')
script.dir <- paste0(root.dir, "/script/R")
input.dir <- paste0(root.dir, "/input")
data.dir <- paste0(root.dir, "/data")
analysis.dir <- paste0(root.dir, '/Analysis')
if (!dir.exists(analysis.dir)) dir.create(analysis.dir)
lulc.dir <- paste0(root.dir, "/Analysis/LULC_maps")
if (!dir.exists(lulc.dir)) dir.create(lulc.dir)
dsi.dir <- paste0(root.dir, "/Analysis/dsi")
if (!dir.exists(dsi.dir)) dir.create(dsi.dir)
harmony.dir <- paste0(root.dir, "/Analysis/harmony")
if (!dir.exists(harmony.dir)) dir.create(harmony.dir)
owl.dir <- paste0(root.dir, '/Analysis/owl_hsi')
if (!dir.exists(owl.dir)) dir.create(owl.dir)
owl.eval.dir <- paste0(root.dir, '/Analysis/owl_hsi/owl_eval')
if (!dir.exists(owl.eval.dir)) dir.create(owl.eval.dir)
kumataka.dir <- paste0(root.dir, '/Analysis/kumataka_hsi')
if (!dir.exists(kumataka.dir)) dir.create(kumataka.dir)
riparian.dir <- paste0(root.dir, '/Analysis/riparian')
if (!dir.exists(riparian.dir)) dir.create(riparian.dir)
energy.dir <- paste0(root.dir, '/Analysis/energy')
if (!dir.exists(energy.dir)) dir.create(energy.dir)
# output.dir <- file.path(root.dir, 'output', Sys.Date())
output.dir <- file.path(root.dir, 'output', '2022-05-23')
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = T)
if (!dir.exists(file.path(output.dir, 'ts-data'))) dir.create(file.path(output.dir, 'ts-data'), recursive = T)


# parameters
agri.ids <- c(203, 301:305) # ID for agricultural land use type
cell.len <- 100 # m
tms.list <- c(1:85) # all time steps
tms.list.harmony <- c(10:85) # maybe not use

# target species
#                  E-needle    D-needle    D-broadfeaf                                                 grass-species
#                  todomatsu   karamatsu   shirakaba   harunire    hannoki     mizunara    yachidamo   bokusou     sasa
spp.name.list <- c("abiesach", "larikaem", "betuplat", "ulmudavi", "alnujapo", "quercris", "fraxmand", "pastgras", "sasagras")

# scenario variables
setwd(root.dir)
file.name.list <- list.files()
file.names <- c(file.name.list)
scenario.name.list <- str_subset(file.names, pattern="_s")  # pick up file names including "_s"
# scenario.name.list <- c("a0_s0.0_clear_long_broad", "a0_s0.0_clear_long_conifer") # pick up only specific scenarios
climate.name.list <- c("MRI_rcp26" , "MRI_rcp85")
# climate.name.list <- c("MRI_rcp85")
package_list <- c("rgdal", "raster", 'terra', "sf", "tidyverse", "readr", "viridis", "rasterVis", "ggthemes", "stringr", 'arrow')
source(file.path(script.dir, 'computeLULC.R'))
source(file.path(script.dir, 'computeOwlHSI.R')) # 12 sec per year
source(file.path(script.dir, 'evaluate_owl_habitat.R'))
source(file.path(script.dir, 'computeKumatakaHSI.R'))
source(file.path(script.dir, 'compute_DSI_harmony_julialang.R'))
source(file.path(script.dir, 'computeRiparianBiomass.R'))
basic.vars <- ls(envir=parent.frame())

