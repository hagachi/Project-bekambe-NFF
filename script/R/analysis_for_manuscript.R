# lineplot evaluation indicators and Jitterplot integrated index
# calculate integrated index using only RCP2.6 scenario
# use after -> analyzeTimeSeriesv3.R
# modify plot_indicators_NC.R
# for marimi thesis Fig.9,11,12

# Initialization ---------------------------------------------------------
root.dir <- rprojroot::find_root('210113_bekambe_NFF_v1.5.Rproj')
script.dir <- paste0(root.dir, "/script/sustR")
source(file.path(script.dir, 'initialization.R'))

library(ggthemr) # https://github.com/Mikata-Project/ggthemr
ggthemr(palette = 'pale')
library(rpart)
library(rpart.plot)
library(partykit)
library(ggparty)
library(ggalluvial)


# Initialize variables ------------------------------------------------------
flux.vars <- c("grass_harvest", "timber_harvest", "yrNEP_TgC", "energy")
# stock.vars <- c("dsi", "owl_hsi", "owl_canoe", "owl_forestBiomass", "cumsumNEP_TgC", 
# stock.vars <- c("diver_withart", "owl_hsi", "owl_canoe", "owl_forestBiomass", "cumsumNEP_TgC",
stock.vars <- c("dsi", "owl_hsi", "owl_canoe", "owl_forestBiomass", "cumsumNEP_TgC", 'sdi_biom',
                "kumataka_hsi", "riparian_biomass_100m", "diver_withart", "diver_ntronly", 
                "vegeNatu_area", "pasture_area", 
                "totalBiomass", "naturalBiomass", "coniferBiomass", "broadlBiomass", 
                "larikaemBiomass", "abiesachBiomass", "betuplatBiomass", "quercrisBiomass", 
                "ulmudaviBiomass", "alnujapoBiomass", "fraxmandBiomass", 
                "viewshedCulture", "viewshedTourism", "viewsimCulture", "viewsimTourism",
                'propnatCulture', 'propnatTourism',
                'naturalness')
# Common / Specific indicators
# common.vars <- c("diver_withart", "cumsumNEP_TgC", 'yrNEP_TgC', "totalBiomass")
# common.vars <- c("dsi", "cumsumNEP_TgC", "totalBiomass", 'sdi_biom')
common.vars <- c("dsi", "yrNEP_TgC", "totalBiomass", 'sdi_biom', 'naturalness')
nn.vars <- c('owl_hsi', 'kumataka_hsi', 'naturalBiomass')
ns.vars <- c('grass_harvest', 'timber_harvest', 'energy')
nc.vars <- c('propnatCulture', 'propnatTourism', 'riparian_biomass_100m')
# nc.vars <- c('viewsimCulture', 'viewsimTourism', 'riparian_biomass_100m')
use.vars <- c(common.vars, nn.vars, ns.vars, nc.vars)
varname.disp.spc <- c('Haitat Suitability Index of Owl', 'Haitat Suitability Index of Eagle',
                      'Total biomass of native trees', 'Pasture yield', 'Timber yield',
                      'Total renewable energy', 'Proportion of natural f_continuescapes near settlements',
                      'Proportion of natural f_continuescapes near tourism spots', "Total biomass of riparian forests")
# Color palette
case.list <- c('NN', 'NS', 'NC', 'NN & NC',
               'NN-NS Pareto', 'NN-NC Pareto', 'NS-NC Pareto', 'NN-NS-NC Pareto',
               'NN-NS Pareto & NN-NC Pareto',
               'Dominated solution', 'Non-Nature positive')
case.col.vals <- c('#3876ab', '#feef00', '#db6565', 'magenta', 
                   '#58acc8', '#dccccf', '#fee1ac', '#1a8064',
                   'cyan',
                   'black', 'grey90')
names(case.col.vals) <- case.list
pointcols <- c('#3876ab', '#feef00', '#db6565', '#58acc8', '#dccccf', '#fee1ac', 'magenta', 'black')
names(pointcols) <- c('NN', 'NS', 'NC', 'NN-NS Pareto', 'NN-NC Pareto', 'NS-NC Pareto', 'NN & NC', 'Dominated solution')
pointsizes <- c(rep(2, 7), 0.5)
names(pointsizes) <- c('NN', 'NS', 'NC', 'NN-NS Pareto', 'NN-NC Pareto', 'NS-NC Pareto', 'NN & NC', 'Dominated solution')

# case.col.vals <- c(brewer_pal(palette = 'Set1')(length(case.list) - 2), 'grey', 'black')


# Read dataset ------------
data <- arrow::read_parquet(file.path(output.dir, "analyzeTimeSeries.parquet")) %>% 
  dplyr::rename(f_continue = 'NFF',
                p_a = 'a',
                p_s = 's',
                p_plant = 'ispasplant',
                f_cut = 'cut',
                f_rotation = 'rotation',
                f_plant = 'plantspp') %>% 
  dplyr::filter(varname %in% use.vars) %>% 
  dplyr::mutate(varname = fct_relevel(varname, use.vars)) %>% 
  dplyr::mutate(p_a = if_else(f_continue == 'nc', 'Riverside', p_a)) %>% 
  # Modify categories
  dplyr::mutate(isBaU = case_when(f_continue == 'nc' ~ TRUE,
                                  TRUE ~ FALSE),
                p_a = case_when(p_a == 'a0' ~ 'No abandonment',
                                p_a == 'a112' ~ 'Half of depop',
                                p_a == 'a223' ~ 'Same as depop',
                                TRUE ~ ''),
                p_s = case_when(p_s == 's0.0' ~ 'No solar',
                                p_s == 's0.5' ~ 'Solar 50%',
                                p_s == 's1.0' ~ 'Solar 100%',
                                TRUE ~ ''),
                p_plant = case_when(p_plant == 'noManagement' ~ 'Natural',
                                    p_plant == 'notplant' ~ 'Natural',
                                    p_plant == 'plant' ~ 'Hardwood',
                                    TRUE ~ ''),
                f_continue = case_when(f_continue == 'around' ~ 'abandon',
                                       f_continue == 'continue' ~ 'continue',
                                       f_continue == 'nc' ~ 'continue',
                                       TRUE ~ ''),
                f_cut = case_when(f_cut == 'selective' ~ 'Selective',
                                  f_cut == 'clear' ~ 'Clear',
                                  TRUE ~ ''),
                f_rotation = case_when(f_rotation == 'long' ~ 'Long',
                                       f_rotation == 'normal' ~ 'Normal',
                                       TRUE ~ ''),
                f_plant = case_when(f_plant == 'broad' ~ 'Hardwood',
                                    f_plant == 'conifer' ~ 'Conifer',
                                    f_plant == 'natural' ~ 'Natural',
                                    TRUE ~ '')) %>% 
  # Modify data name 
  mutate(varname.disp = case_when(varname == 'dsi' ~ 'Dissimilarity-based Satoyama Index',
                                  varname == 'yrNEP_TgC' ~ 'NEP',
                                  varname == 'totalBiomass' ~ 'Total Tree Biomass',
                                  varname == 'sdi_biom' ~ "Simpson's Diversity Index of Tree Biomass",
                                  varname == 'naturalness' ~ '% of Natural f_continuescape',
                                  varname == 'owl_hsi' ~ 'Haitat Suitability Index of Owl',
                                  varname == 'kumataka_hsi' ~ 'Haitat Suitability Index of Eagle',
                                  varname == 'naturalBiomass' ~ 'Total biomass of native trees',
                                  varname == 'grass_harvest' ~ 'Pasture yield',
                                  varname == 'timber_harvest' ~ 'Timber yield',
                                  varname == 'energy' ~ 'Total renewable energy',
                                  varname == 'propnatCulture' ~ 'Proportion of natural f_continuescapes near settlements',
                                  varname == 'propnatTourism' ~ 'Proportion of natural f_continuescapes near tourism spots',
                                  varname == 'riparian_biomass_100m' ~ "Total biomass of riparian forests")) %>%
  mutate(varname.disp = fct_relevel(varname.disp, 'Dissimilarity-based Satoyama Index',
                                    'NEP','Total Tree Biomass', "Simpson's Diversity Index of Tree Biomass", '% of Natural f_continuescape',
                                    varname.disp.spc)) %>% 
  # 需要の5倍でキャップをかける
  dplyr::mutate(val = if_else(varname == "energy" & val>1.285*5*10^3, 1.285*5*10^3, val))
str(data) # Check if the val and Time col is numeric or not.
naniar::miss_var_summary(data)


## Define Functions -----
PlotAllScenarios <-  function(plot.data.df, plt.climate){
  plt <- plot.data.df%>%
    filter(climate == plt.climate, Time > 1) %>% 
    ggplot() +
    geom_line(aes(x = 2015 + Time, y = val, 
                  group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, varname),
                  color = f_continue),
              size = .2, alpha = 1) +
    labs(title = paste0('all_indices_', plt.climate), x = 'Year', y = 'Value') +
    scale_color_tableau() +
    facet_wrap(~varname.disp , scales = 'free')
  return(plt)
}

ComputeFluxIdx <- function(df) {
  data.frame(Time = df$Time, val = cumsum(df$val))
}

PassFluxIdx <- function(df) {
  df %>% 
    rename(val.clust = val)
}

ComputeStockIdx <- function(df) {
  data.frame(Time = df$Time[3:(85-2)], val = cumsum(c(0, diff(df$val))))
}

DiffStockIdx <- function(df) {
  data.frame(Time = df$Time, val.clust = c(0, diff(df$val)))
}

RollMeanIdx <- function(df) {
  data.frame(Time = df$Time, val = zoo::rollmean(df$val, k = 5, fill = NA))
}

PassIdx <- function(df) {
  df
}

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

zscale <- function(x) {
  (x - mean(x)) / sd(x)
}

DetectNaturePositives <- function(data.integ.indices) {
  buf.df <- data.integ.indices %>%
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>%
    dplyr::select(idxtype, Time, isBaU, scenario, val.integ) %>%
    dplyr::filter(idxtype == 'common') %>% 
    dplyr::mutate(decades = case_when(Time + 2015 >= 2016 & Time + 2015 < 2029 ~ 2015,
                                      Time + 2015 >=  2030 & Time + 2015 < 2040 ~ 2030,
                                      Time + 2015 >=  2050 & Time + 2015 < 2060 ~ 2050,
                                      Time + 2015 >=  2091 & Time + 2015 <= 2100 ~ 2090,
                                      TRUE ~ as.double(NA))) %>% 
    dplyr::filter(!is.na(decades)) %>% 
    dplyr::group_by(isBaU, idxtype, scenario, decades) %>% 
    dplyr::summarise(val.integ.mean = mean(val.integ), 
                     val.integ.median = median(val.integ), .groups = 'drop')
  base.df <- buf.df %>%
    filter(isBaU, decades == 2015) %>%
    rename(val.integ.mean.bau = val.integ.mean,
           val.integ.median.bau = val.integ.median) %>%
    select(-isBaU, -scenario)
  candidates.df <- buf.df
  scs <- unique(candidates.df$scenario)
  flags <- c()
  for (sc in scs) {
    sc.df <- candidates.df %>%
      filter(scenario == sc) %>%
      left_join(base.df, by = c('idxtype')) %>%
      mutate(val.positive = (val.integ.mean - val.integ.mean.bau) >= 0)
    sc15.df <- filter(sc.df, decades.x == 2030) # 2030-2039
    sc35.df <- filter(sc.df, decades.x == 2050) # 2050-2059
    sc85.df <- filter(sc.df, decades.x == 2090) # 2091-2100
    # update flags
    flag15 <- 'n'; flag35 <- 'n'; flag85 <- 'n'
    if (sc15.df$val.positive) flag15 <- 'p'
    if (sc35.df$val.positive) flag35 <- 'p'
    if (sc85.df$val.positive) flag85 <- 'p'
    flags <- append(flags, paste0(flag15, flag35, flag85))
  }
  return(list(scenarios = scs, flags = flags, basedf = base.df))
}

IdentifyPareto2d <- function(id, x, y, isplot=FALSE) {
  # Change direction from maximize to minimize
  x <- -x; y <- -y
  x.sort <- sort(x)
  y.sort <- y[order(x)]
  id.sort <- id[order(x)]
  y.min <- max(y)
  pareto.ids <- c(); x.pareto <- c(); y.pareto <- c()
  for(i in 1:length(x.sort)) {
    if (y.sort[i] <= y.min) {
      y.min <- y.sort[i]
      pareto.ids <- append(pareto.ids, id.sort[i])
      x.pareto <- append(x.pareto, x.sort[i])
      y.pareto <- append(y.pareto, y.sort[i])
    }
  }
  # modify order
  x.pareto <- -x.pareto
  y.pareto <- -y.pareto
  # Check result
  if (isplot) {
    plot(-x, -y)
    lines(x.pareto, y.pareto, col="red")
  }
  # top 10% Pareto fronts
  x.pareto.top10 <- pareto.ids[x.pareto >= quantile(x.pareto, 0.9)]
  y.pareto.top10 <- pareto.ids[y.pareto >= quantile(y.pareto, 0.9)]
  # the other Pareto fronts
  xy.pareto <- setdiff(pareto.ids, union(x.pareto.top10, y.pareto.top10))
  # return
  out <- list(all = pareto.ids, 
              x.top10 = x.pareto.top10, 
              y.top10 = y.pareto.top10, 
              other = xy.pareto)
  return(out)
}

# Identify NFF class
IdentifyNFFclass <- function(df) {
  classes <- setdiff(unique(df$class), 'Dominated solution')
  if (length(classes) > 0) {
    return(paste0(classes, collapse = ' & '))
  } else {
    return('Dominated solution')
  }
}



# Main procedure ---------------------------------------------------------------
for (climate.name in climate.name.list) {
  # 1. Time series Check ---------------------------------------------------------
  ## Plot indicators for all management scenarios --------
  PlotAllScenarios(data, climate.name)

  # 2. Compute indicator values -------------
  # Compute indices for rcp2.6
  ## Compute indices ------------
  data.indices <- data %>%
    dplyr::filter(climate == climate.name) %>% 
    dplyr::select(-class) %>%
    # identify flux / stock and nn / ns / nc
    dplyr::mutate(vartype = case_when(varname %in% flux.vars ~ 'flux',
                                      varname %in% stock.vars ~ 'stock',
                                      TRUE ~ as.character(NA)),
                  idxtype = case_when(varname %in% common.vars ~ 'common',
                                      varname %in% nn.vars ~ 'nn',
                                      varname %in% ns.vars ~ 'ns',
                                      varname %in% nc.vars ~ 'nc',
                                      TRUE ~ as.character(NA))) %>%
    dplyr::mutate(idxtype = fct_relevel(idxtype, 'common', 'nn', 'ns', 'nc')) %>% 
    dplyr::filter(Time > 1, !is.na(val))
  
  
  # Data check
  ## for Data
  # data.indices %>% 
  #   ggplot(aes(x = Time, y = val, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate),
  #              color = f_continue)) +
  #   geom_line(size = .1) +
  #   scale_color_tableau() +
  #   labs(title = 'TS for publication') +
  #   facet_wrap(~varname , scales = 'free')
  
  
  ## max-min scaling -----------------
  data.indices.scaled <- data.indices %>%
    filter(!is.na(idxtype)) %>% 
    dplyr::group_by(varname, varname.disp, climate) %>%
    # dplyr::mutate(val = minMax(val),
    #               val.clust = minMax(val.clust)) %>% 
    dplyr::mutate(val = minMax(val)) %>%
    ungroup()
  # data.indices.scaled %>% 
  #   ggplot(aes(x = Time, y = val, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate), color = f_continue)) +
  #   geom_line(size = .1) +
  #   scale_color_tableau() +
  #   labs(title = 'TS for clustering') +
  #   facet_wrap(~varname , scales = 'free')

  
  ## Compute mean indices ---------
  data.integ.indices <- data.indices.scaled %>% 
    group_by(idxtype, isBaU, f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, Time) %>% 
    summarise(val.integ = mean(val), .groups = 'drop')
  
  # ggplot(data.integ.indices, 
  #        aes(x = Time + 2015, y = val.integ,
  #            group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate),
  #            color = f_continue)) +
  #   geom_line(size = .1) +
  #   # geom_vline(xintercept = c(2030, 2050, 2100), linetype = 'dashed') +
  #   scale_color_tableau() +
  #   labs(x = 'Year', y = 'Mean score', 
  #        caption = 'Each scores show mean values among four (common) or three (nn, ns, and nc) indicators.') +
  #   facet_grid(climate~idxtype, scales = 'free')
  # 
  
  # 3. Filtering Nature positives -------------------------------------------------
  buf.np.lst <- DetectNaturePositives(data.integ.indices)
  scs <- buf.np.lst$scenarios; flags <- buf.np.lst$flags; base.df <- buf.np.lst$basedf
  np.df <- tibble(scenario = scs, 
                  nplabel = flags) %>% 
    dplyr::mutate(np = case_when(nplabel == 'ppp' ~ '2030s, 2050s, and 2090s',
                                 nplabel == 'pnp' ~ '2030s and 2090s',
                                 nplabel == 'ppn' ~ '2030s and 2050s',
                                 nplabel == 'npp' ~ '2050s and 2090s',
                                 nplabel == 'pnn' ~ '2030s',
                                 nplabel == 'npn' ~ '2050s',
                                 nplabel == 'nnp' ~ '2090s',
                                 nplabel == 'nnn' ~ 'Always < BaU',
                                 TRUE ~ as.character(NA))) %>% 
    dplyr::mutate(np = fct_relevel(np, '2030s and 2090s', '2030s', '2090s', 'Always < BaU',
                                   '2030s, 2050s, and 2090s', '2030s and 2050s', '2050s and 2090s', '2050s')) %>% 
    dplyr::arrange(np) %>% 
    dplyr::mutate(isNP = if_else(np %in% c('2030s, 2050s, and 2090s', '2030s and 2050s'), TRUE, FALSE))
  table(np.df$np)
  table(np.df$isNP)
  write_csv(np.df, file.path(output.dir, paste0('data_nature-positive-scenarios_', climate.name, '.csv')))
  ## Merge with raw data --------
  data.indices.nff.candidates <- data.indices %>%
    dplyr::filter(!is.na(idxtype)) %>% 
    dplyr::filter(climate == climate.name) %>% 
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>% 
    dplyr::left_join(np.df, by = 'scenario') %>% 
    dplyr::mutate(data.type = 'Raw Indices') %>% 
    dplyr::select(-varname)
  ## Merge with scaled raw data --------
  data.indices.scaled.nff.candidates <- data.indices.scaled %>% 
    dplyr::filter(climate == climate.name) %>% 
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>% 
    dplyr::left_join(np.df, by = 'scenario') %>% 
    dplyr::mutate(data.type = 'Raw Scaled Indices') %>% 
    dplyr::select(-varname)
  ## Merge with integ data -------------
  data.integ.nff.candidates <- data.integ.indices %>% 
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>% 
    dplyr::left_join(np.df, by = 'scenario') %>% 
    dplyr::mutate(data.type = 'Integrated Indices') %>% 
    dplyr::rename(val = val.integ)
  data.nff.candidates <- bind_rows(data.integ.nff.candidates,
                                   data.indices.scaled.nff.candidates,
                                   data.indices.nff.candidates)
  naniar::miss_var_summary(data.nff.candidates)
  write_csv(data.nff.candidates, file.path(output.dir, paste0('data_nature-positive-scenarios_', climate.name, '_with-indices.csv')))
  ## Summarise ----------
  (data.nff.summary <- data.nff.candidates %>% 
    # Summarise by decades
    dplyr::mutate(decades = case_when(Time + 2015 < 2030 ~ 2015,
                                      Time + 2015 >=  2030 & Time + 2015 < 2040 ~ 2030,
                                      Time + 2015 >= 2050 & Time + 2015 < 2060 ~ 2050,
                                      Time + 2015 >= 2091 & Time + 2015 <= 2100 ~ 2090,
                                      TRUE ~ as.double(NA))) %>% 
    dplyr::filter(!is.na(decades)) %>% 
    dplyr::group_by(idxtype, data.type, varname.disp, decades) %>% 
    dplyr::summarise(val.mean = mean(val, na.rm = T),
                     val.sd = sd(val, na.rm = T),
                     val.min = min(val),
                     val.median = median(val),
                     val.max = max(val), .groups = 'drop'))
  write_csv(data.nff.summary, file.path(output.dir, paste0('data_nature-positive-scenarios_', climate.name, '_with-indices-summary.csv')))
  
  
  
  # 4. Result 1: Nature positives ------------------------------------------------
  iccolors <- c('2030s, 2050s, and 2090s' = 'skyblue', 
                '2030s and 2050s' = 'grey', 
                '2050s and 2090s' = 'grey', 
                '2050s' = 'grey', 
                '2030s and 2090s' = 'grey',
                '2030s' = 'grey',
                '2090s' = 'grey',
                'Always < BaU' = 'grey')
  icsizes <- c('2030s, 2050s, and 2090s' = .7, 
               '2030s and 2050s' = .7, '2050s and 2090s' = .5, '2050s' = .5, 
               '2030s and 2090s' = .5, '2030s' = .5, '2090s' = .5,'Always < BaU' = .5)
  ictypes <- c('2030s, 2050s, and 2090s' = 'solid', 
               '2030s and 2050s' = 'solid', '2050s and 2090s' = 'dashed', '2050s' = 'dashed', 
               '2030s and 2090s' = 'dashed', '2030s' = 'dashed', '2090s' = 'dashed','Always < BaU' = 'dashed')
  iccolors <- iccolors[names(iccolors) %in% unique(data.integ.nff.candidates$np)]
  icsizes <- icsizes[names(icsizes) %in% unique(data.integ.nff.candidates$np)]
  ictypes <- ictypes[names(ictypes) %in% unique(data.integ.nff.candidates$np)]
  
  (plt.res1.sub <- data.indices.scaled %>% 
      filter(idxtype == 'common', climate == climate.name) %>% 
      mutate(varname.disp = case_when(varname == 'dsi' ~ 'Dissimilarity-based Satoyama Index',
                                      varname == 'yrNEP_TgC' ~ 'NEP',
                                      varname == 'totalBiomass' ~ 'Total Tree Biomass',
                                      varname == 'sdi_biom' ~ "Simpson's Diversity Index of Tree Biomass",
                                      varname == 'naturalness' ~ '% of Natural f_continuescapes')) %>% 
      mutate(varname.disp = fct_relevel(varname.disp, 'Dissimilarity-based Satoyama Index',
                                        'NEP','Total Tree Biomass', "Simpson's Diversity Index of Tree Biomass", '% of Natural f_continuescapes')) %>% 
      dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>% 
      dplyr::left_join(np.df, by = 'scenario') %>% 
      ggplot(aes(x = Time + 2015, y = val, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate), color = np, size = np, linetype = np), alpha = .5) +
      annotate("rect", xmin = 2016, xmax = 2029, ymin = -Inf, ymax = Inf, alpha = 0.2, fill="grey") +
      annotate("rect", xmin = 2030, xmax = 2039, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      annotate("rect", xmin = 2050, xmax = 2059, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      annotate("rect", xmin = 2091, xmax = 2100, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      geom_line(alpha = .5) +
      scale_color_manual(values = iccolors) +
      scale_size_manual(values = icsizes) +
      scale_linetype_manual(values = ictypes) +
      scale_x_continuous(expand = c(.01, .01), breaks = seq(2020, 2100, by = 10), labels = seq(2020, 2100, by = 10)) +
      scale_y_continuous(expand = c(.01, .01), limits = c(0, 1)) +
      labs(x = 'Year', y = 'Min-max scaled scores', title = 'Individual common indices', subtitle = climate.name) +
      facet_wrap(~varname.disp, ncol = 2, scales = 'free') +
      theme(panel.border = element_rect(color = 'grey', fill = NA),
            axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size = 18)))
  
  (plt.res1.main <- ggplot() +
      annotate("rect", xmin = 2016, xmax = 2029, ymin = -Inf, ymax = Inf, alpha = 0.2, fill="grey") +
      annotate("rect", xmin = 2030, xmax = 2039, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      annotate("rect", xmin = 2050, xmax = 2059, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      annotate("rect", xmin = 2091, xmax = 2100, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      geom_line(data = filter(data.integ.nff.candidates, idxtype == 'common'),
                aes(x = Time + 2015, y = val, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, np), color = np, size = np, linetype = np), alpha = .5) +
      scale_color_manual(values = iccolors) +
      scale_size_manual(values = icsizes) +
      scale_linetype_manual(values = ictypes) +
      geom_hline(yintercept = base.df$val.integ.mean.bau, color = 'black', linetype = 'dashed', size = 1) +
      scale_x_continuous(expand = c(.01, .01), breaks = seq(2020, 2100, by = 10), labels = seq(2020, 2100, by = 10)) +
      scale_y_continuous(expand = c(.01, .01), limits = c(0, 1)) +
      labs(x = 'Year', y = 'Integrated common index', linetype = '>= current', color = '>= current', size = '>= current', 
           title = 'Integrated common index', subtitle = climate.name, caption = 'Colored lines show the nature positive cases.') +
      theme(panel.border = element_rect(color = 'grey', fill = NA),
            text = element_text(size = 16),
            legend.text = element_text(size = 8),
            legend.position = 'right'))
  ggsave(file.path(output.dir, paste0('res1_nature-positive-timeseries', climate.name, '_sub.pdf')), plt.res1.sub, width = 16, height = 12)
  ggsave(file.path(output.dir, paste0('res1_nature-positive-timeseries', climate.name, '_main-only.pdf')), plt.res1.main, width = 8, height = 6)

  # for wbf2022
  (ggplot() +
      geom_line(data = data.integ.nff.candidates,
                aes(x = Time + 2015, y = val, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, np), color = np, size = np, linetype = np), alpha = .5, size = .5, color = 'grey30') +
      scale_x_continuous(expand = c(.01, .01), breaks = seq(2020, 2100, by = 10), labels = seq(2020, 2100, by = 10)) +
      scale_y_continuous(expand = c(.01, .01), limits = c(0, 1)) +
      facet_grid(~idxtype) +
      labs(x = 'Year', y = 'Mean value', linetype = '>= current', color = '>= current', size = '>= current', 
           title = 'Integrated value', subtitle = climate.name, caption = 'Colored lines show the nature positive cases.') +
      theme(panel.border = element_rect(color = 'grey', fill = NA),
            text = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text = element_text(size = 8),
            legend.position = 'right'))
  
  # 5. Supplement result: Changes in NFF specific indicators ----
  data.res2 <- data.indices.scaled %>% 
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>% 
    dplyr::left_join(np.df, by = 'scenario') %>%
    dplyr::select(-scenario) %>% 
    mutate(varname.disp = case_when(varname == 'owl_hsi' ~ 'Haitat Suitability Index of Owl',
                                    varname == 'kumataka_hsi' ~ 'Haitat Suitability Index of Eagle',
                                    varname == 'naturalBiomass' ~ 'Total biomass of native trees',
                                    varname == 'grass_harvest' ~ 'Pasture yield',
                                    varname == 'timber_harvest' ~ 'Timber yield',
                                    varname == 'energy' ~ 'Cummulative renewable energy',
                                    varname == 'propnatCulture' ~ 'Proportion of natural landscapes near settlements',
                                    varname == 'propnatTourism' ~ 'Proportion of natural landscapes near tourism spots',
                                    varname == 'riparian_biomass_100m' ~ "Total biomass of riparian forests")) %>%
    mutate(varname.disp = fct_relevel(varname.disp, varname.disp.spc))
  (plt.res2.sub <- ggplot() +
     annotate("rect", xmin = 2030, xmax = 2039, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
     annotate("rect", xmin = 2050, xmax = 2059, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
     annotate("rect", xmin = 2091, xmax = 2100, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
     geom_line(data = filter(data.res2, idxtype != 'common'), 
               aes(x = Time + 2015, y = val, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = np, size = np)) +
     scale_color_manual(values = iccolors) +
     scale_size_manual(values = icsizes) +
     scale_x_continuous(expand = c(.01, .01), breaks = seq(2020, 2100, by = 10), labels = seq(2020, 2100, by = 10)) +
     scale_y_continuous(expand = c(.01, .01), limits = c(0, 1)) +
     labs(x = 'Year', y = 'Min-max scaled scores', title = 'Individual NFF specific indices', subtitle = climate.name,
          color = 'Top 10%', size = 'Top 10%') +
     facet_wrap(~varname.disp, dir = 'v', scales = 'free') +
     theme(panel.border = element_rect(color = 'grey', fill = NA),
           text = element_text(size = 16),
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = 'bottom'))
  (plt.res2.main <- ggplot() +
      annotate("rect", xmin = 2030, xmax = 2039, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      annotate("rect", xmin = 2050, xmax = 2059, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      annotate("rect", xmin = 2091, xmax = 2100, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
      geom_line(data = filter(data.integ.nff.candidates, idxtype != 'common'), 
                aes(x = Time + 2015, y = val, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = np, size = np)) +
      scale_color_manual(values = iccolors) +
      scale_size_manual(values = icsizes) +
      scale_x_continuous(expand = c(.01, .01), breaks = seq(2020, 2100, by = 10), labels = seq(2020, 2100, by = 10)) +
      scale_y_continuous(expand = c(.01, .01), limits = c(0, 1)) +
      labs(x = 'Year', y = 'Min-max scaled scores', title = 'Integrated NFF specific indices', subtitle = climate.name,
           caption = 'Colored lines show the top 10% of nature positive cases for each decade.',
           color = 'Top 10%', size = 'Top 10%') +
      facet_wrap(~idxtype, ncol = 2, scales = 'free') +
      theme(panel.border = element_rect(color = 'grey', fill = NA),
            text = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = c(.8, .2)))
  ggsave(file.path(output.dir, paste0('res2_specific_indicators_', climate.name, '_sub.pdf')), plt.res2.sub, width = 16, height = 16)
  ggsave(file.path(output.dir, paste0('res2_specific_indicators_', climate.name, '_main.pdf')), plt.res2.main, width = 8, height = 8)
  
  
  # 6. Result 3: Pareto front analysis -----------------------------------------------------
  scatter.data <- data.integ.indices %>%
    # Filter nature positives
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>%
    dplyr::left_join(np.df, by = 'scenario') %>%
    dplyr::filter(np %in% c('2030s, 2050s, and 2090s', '2030s and 2050s')) %>%
    dplyr::filter(idxtype != 'common') %>% 
    # Summarise by decades
    dplyr::mutate(decades = case_when(Time + 2015 >=  2030 & Time + 2015 < 2040 ~ 2030,
                                      Time + 2015 >= 2050 & Time + 2015 < 2060 ~ 2050,
                                      Time + 2015 >= 2091 & Time + 2015 <= 2100 ~ 2090,
                                      TRUE ~ as.double(NA))) %>% 
    dplyr::filter(!is.na(decades)) %>% 
    dplyr::group_by(np, idxtype, scenario, decades) %>% 
    dplyr::summarise(val.integ.mean = mean(val.integ), .groups = 'drop') %>% 
    # Convert to wider format
    pivot_wider(names_from = idxtype, values_from = val.integ.mean)

  ## Identify two dimentional pareto fronts and top10% indices ------
  pareto_dfs <- list(); cases_dfs <- list()
  ts <- c(2030, 2050, 2090)
  for (t in ts) {
    df <- scatter.data %>% 
      filter(decades == t) %>% 
      dplyr::select('scenario', 'nn', 'ns', 'nc')
    pareto.nnns.scenarios <- IdentifyPareto2d(df$scenario, df$nn, df$ns)
    pareto.nnnc.scenarios <- IdentifyPareto2d(df$scenario, df$nn, df$nc)
    pareto.nsnc.scenarios <- IdentifyPareto2d(df$scenario, df$ns, df$nc)
    # Merge dataframes
    buf.nnns.df <- df %>% 
      left_join(data.frame(scenario = c(pareto.nnns.scenarios$x.top10, pareto.nnns.scenarios$y.top10, pareto.nnns.scenarios$other),
                           class = c(rep('NN', length(pareto.nnns.scenarios$x.top10)), 
                                    rep('NS', length(pareto.nnns.scenarios$y.top10)), 
                                    rep('NN-NS Pareto', length(pareto.nnns.scenarios$other)))), by = 'scenario') %>% 
      replace_na(list(class = 'Dominated solution'))
    buf.nnnc.df <- df %>% 
      {if (!(length(pareto.nnnc.scenarios$x.top10) == 1 & length(pareto.nnnc.scenarios$y.top10) == 1 &
             pareto.nnnc.scenarios$x.top10 == pareto.nnnc.scenarios$y.top10)) {
        left_join(., data.frame(scenario = c(pareto.nnnc.scenarios$x.top10, pareto.nnnc.scenarios$y.top10, pareto.nnnc.scenarios$other),
                                class = c(rep('NN', length(pareto.nnnc.scenarios$x.top10)), 
                                      rep('NC', length(pareto.nnnc.scenarios$y.top10)), 
                                      rep('NN-NC Pareto', length(pareto.nnnc.scenarios$other)))), by = 'scenario')
      } else {
        left_join(., data.frame(scenario = pareto.nnnc.scenarios$x.top10,
                                class = 'NN & NC'), by = 'scenario')
      }} %>% 
      replace_na(list(class = 'Dominated solution'))
    buf.nsnc.df <- df %>% 
      left_join(data.frame(scenario = c(pareto.nsnc.scenarios$x.top10, pareto.nsnc.scenarios$y.top10, pareto.nsnc.scenarios$other),
                           class = c(rep('NS', length(pareto.nsnc.scenarios$x.top10)), 
                                    rep('NC', length(pareto.nsnc.scenarios$y.top10)), 
                                    rep('NS-NC Pareto', length(pareto.nsnc.scenarios$other)))), by = 'scenario') %>% 
      replace_na(list(class = 'Dominated solution'))
    # NN vs NS
    (plt.pareto.nnns <- ggplot() +
        geom_line(data = filter(buf.nnns.df, class !='Dominated solution'),　aes(x = nn, y = ns), color = 'grey20') +
        geom_point(data = buf.nnns.df, aes(x = nn, y = ns, color = class, size = class)) +
        ggpubr::stat_cor(data = buf.nnns.df, aes(x = nn, y = ns, label = ..r.label..), 
                         method = "pearson", label.x = 1, label.y = 1, hjust = 1, vjust = 1) +
        scale_color_manual(values = pointcols, drop = T) +
        scale_size_manual(values = pointsizes) +
        coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(x = 'Nature for Nature', y = 'Nature for Society', title = paste('Year:', t)) +
        theme(panel.border = element_rect(color = 'grey', fill = NA), legend.position = 'none'))
    # NS vs NC
    (plt.pareto.nsnc <- ggplot() +
        geom_line(data = filter(buf.nsnc.df, class !='Dominated solution'),　aes(x = ns, y = nc), color = 'grey20') +
        geom_point(data = buf.nsnc.df,　aes(x = ns, y = nc, color = class, size = class)) +
        ggpubr::stat_cor(data = buf.nsnc.df, aes(x = ns, y = nc, label = ..r.label..), 
                         method = "pearson", label.x = 1, label.y = 1, hjust = 1, vjust = 1) +
        scale_color_manual(values = pointcols, drop = T) +
        scale_size_manual(values = pointsizes) +
        coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(x = 'Nature for Society', y = 'Nature as Culture') +
        theme(panel.border = element_rect(color = 'grey', fill = NA), legend.position = 'none'))
    # NN vs NC
    (plt.pareto.nnnc <- ggplot() +
        geom_line(data = filter(buf.nnnc.df, class !='Dominated solution'),　aes(x = nn, y = nc), color = 'grey20') +
        geom_point(data = buf.nnnc.df,　aes(x = nn, y = nc, color = class, size = class)) +
        ggpubr::stat_cor(data = buf.nnnc.df, aes(x = nn, y = nc, label = ..r.label..), 
                         method = "pearson", label.x = 0, label.y = 1, hjust = 0, vjust = 1) +
        scale_color_manual(values = pointcols, drop = T) +
        scale_size_manual(values = pointsizes) +
        coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(x = 'Nature for Nature', y = 'Nature as Culture') +
        theme(panel.border = element_rect(color = 'grey', fill = NA), legend.position = 'none'))
    # Save 2d pareto frontier
    if (which(ts == t) == 1) {
      plt.pareto <- plt.pareto.nnns + plt.pareto.nsnc + plt.pareto.nnnc
    } else {
      plt.pareto <- plt.pareto / (plt.pareto.nnns + plt.pareto.nsnc + plt.pareto.nnnc)
    }
    
    ## for 3D pareto frontier
    pareto_dfs[[which(ts == t)]] <- data.frame(Time = t,
                                               scenario = c(pareto.nnns.scenarios$all, pareto.nnnc.scenarios$all, pareto.nsnc.scenarios$all),
                                               cases = c(rep('nnns', length(pareto.nnns.scenarios$all)),
                                                         rep('nnnc', length(pareto.nnnc.scenarios$all)),
                                                         rep('nsnc', length(pareto.nsnc.scenarios$all))),
                                               ispareto = 'Pareto solution')
    ## for NFF classification
    cases_dfs[[which(ts == t)]] <- bind_rows(mutate(buf.nnns.df, case = 'ns'),
                                             mutate(buf.nnnc.df, case = 'nc'),
                                             mutate(buf.nnns.df, case = 'sc')) %>% 
      mutate(decades = t)
  }
  plt.pareto <- plt.pareto +
    patchwork::plot_annotation(title = 'Pareto front between NFF integrated indices',
                               subtitle = climate.name,
                               caption = 'Large plots and lines show non-dominated solutions and pareto front, respectively. 
                               Colors show the classification in NFF triangles in Fig M3.
                               The small black plots are the dominated solutions.')
  ggsave(file.path(output.dir, paste0('res3_pareto_2d_', climate.name, '.pdf')), plt.pareto, width = 8, height = 10)
  pareto_all_df <- bind_rows(pareto_dfs)
  write_csv(pareto_all_df, file.path(output.dir, paste0('data_pareto-solutions_', climate.name, '.csv')))
  cases_all_df <- bind_rows(cases_dfs)
  
  
  ## 3D pareto front ----------
  pareto_df <- pareto_all_df %>% 
    distinct(Time, scenario, ispareto)
  pareto3d.data <- data.integ.indices %>%
    # Filter nature positives
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>%
    dplyr::left_join(np.df, by = 'scenario') %>%
    dplyr::filter(np %in% c('2030s, 2050s, and 2090s', '2030s and 2050s')) %>% 
    dplyr::filter(idxtype != 'common') %>% 
    # Summarise by decades
    dplyr::mutate(decades = case_when(Time + 2015 >=  2030 & Time + 2015 < 2040 ~ 2030,
                                      Time + 2015 >= 2050 & Time + 2015 < 2060 ~ 2050,
                                      Time + 2015 >= 2091 & Time + 2015 <= 2100 ~ 2090,
                                      TRUE ~ as.double(NA))) %>% 
    dplyr::filter(!is.na(decades)) %>% 
    dplyr::group_by(idxtype, scenario, decades) %>% 
    dplyr::summarise(val.integ.mean = mean(val.integ), .groups = 'drop') %>% 
    # Convert to wider format
    tidyr::pivot_wider(names_from = idxtype, values_from = val.integ.mean) %>% 
    # Identify pareto solutions
    dplyr::left_join(pareto_df, by = c('decades' = 'Time', 'scenario')) %>% 
    tidyr::replace_na(list(ispareto = 'The others')) %>% 
    dplyr::mutate(isparetosize = if_else(ispareto == 'Pareto solution', 20, 5),
                  decades = factor(decades))
  write_csv(pareto3d.data, file.path(output.dir, paste0('data_pareto-3d_', climate.name, '.csv')))
  xpanel.data <- mutate(pareto3d.data, nc = 0, isparetosize = .1)
  ypanel.data <- mutate(pareto3d.data, nn = 1, isparetosize = .1)
  zpanel.data <- mutate(pareto3d.data, ns = 0, isparetosize = .1)
  pltcols <- c('skyblue', 'firebrick3', 'deepskyblue4')
  (fig <- plot_ly() %>% 
      add_markers(data = pareto3d.data,
                  x = ~nn, y = ~ns, z = ~nc, type = 'scatter3d',
                  color = ~decades, symbol = ~ispareto, size = ~isparetosize,
                  symbols = c('circle', 'circle'), colors = pltcols, text = ~scenario,
                  hovertemplate = 'scenario: %{text}') %>%
      add_markers(data = xpanel.data, x = ~nn, y = ~ns, z = ~nc, type = 'scatter3d', 
                  marker = list(size = 1, color = 'rgb(190,190,190,.0)')) %>% 
      add_markers(data = ypanel.data, x = ~nn, y = ~ns, z = ~nc, type = 'scatter3d', 
                  marker = list(size = 1, color = 'rgb(190,190,190,.0)')) %>% 
      add_markers(data = zpanel.data, x = ~nn, y = ~ns, z = ~nc, type = 'scatter3d', 
                  marker = list(size = 1, color = 'rgb(190,190,190,.0)')) %>% 
      layout(scene = list(xaxis = list(title = 'Nature for Nature'),
                          yaxis = list(title = 'Nature for Society'),
                          zaxis = list(title = 'Nature as Culture'))))
  htmlwidgets::saveWidget(as_widget(fig), file.path(output.dir, paste0('res3_pareto_3d_', climate.name, '.html')))
  
  
  # 7. Supplement Result individual pareto front -------------------------------------------------------------------
  ## pareto frontier between indivitual plots ----
  scatter.data.all <- data.indices.scaled %>%
    # Filter nature positives
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>%
    dplyr::left_join(np.df, by = 'scenario') %>%
    filter(np != 'Always < BaU') %>%
    # Summarise by decades
    dplyr::mutate(decades = case_when(Time + 2015 >=  2030 & Time + 2015 < 2040 ~ 2030,
                                      Time + 2015 >= 2050 & Time + 2015 < 2060 ~ 2050,
                                      Time + 2015 >= 2091 & Time + 2015 <= 2100 ~ 2090,
                                      TRUE ~ as.double(NA))) %>% 
    dplyr::filter(!is.na(decades)) %>% 
    dplyr::group_by(varname.disp, scenario, decades) %>% 
    dplyr::summarise(val.mean = mean(val), .groups = 'drop') %>% 
    # Convert to wider format
    pivot_wider(names_from = varname.disp, values_from = val.mean)
  # Plot pareto front
  pareto_dfs <- list()
  ts <- c(2030, 2050, 2090)
  for (t in ts) {
    df <- scatter.data.all %>% 
      filter(decades == t)
    ## 2 dimentional pareto fronts
    pdf(file.path(output.dir, paste0('fig_supplement_', t, '_', climate.name, '.pdf')), width = 16, height = 9)
    psych::pairs.panels(df[, 3:15], xlim = c(0, 1), ylim = c(0, 1), ellipses = FALSE, 
                        main = paste('Year:', t, ' of ', climate.name))
    dev.off()
  }
  
  
  # 8. Result 4. Decision tree analysis ------------
  nffclass.df <- cases_all_df %>% 
    group_nest(scenario, decades, nn, ns, nc) %>% 
    mutate(nffclass = map(data, IdentifyNFFclass)) %>% 
    unnest(nffclass) %>% select(-data) %>% 
    # modify special case
    mutate(nffclass = if_else(nffclass == 'NN & NN & NC', 'NN & NC', nffclass))
  table(nffclass.df$nffclass)
  ## Nature positive (NN, NS, NC or pareto) or Non-nature positive ------
  for (tms in c(2030, 2050, 2090)) {
    all.part.df <- data.integ.indices %>%
      # Filter nature positives
      dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>%
      dplyr::left_join(np.df, by = 'scenario') %>%
      dplyr::filter(idxtype != 'common') %>% 
      # Summarise by decades
      dplyr::mutate(decades = case_when(Time + 2015 >=  2030 & Time + 2015 < 2040 ~ 2030,
                                        Time + 2015 >= 2050 & Time + 2015 < 2060 ~ 2050,
                                        Time + 2015 >= 2091 & Time + 2015 <= 2100 ~ 2090,
                                        TRUE ~ as.double(NA))) %>% 
      dplyr::filter(decades == tms) %>% 
      dplyr::group_by(idxtype, scenario) %>% 
      dplyr::summarise(val.integ.mean = mean(val.integ), .groups = 'drop') %>% 
      # Convert to wider format
      pivot_wider(names_from = idxtype, values_from = val.integ.mean) %>% 
      # Join classes
      dplyr::left_join(dplyr::filter(nffclass.df, decades == tms) %>% 
                         dplyr::select(scenario, decades, nffclass), by = 'scenario') %>% 
      dplyr::select(scenario, nffclass) %>% 
      # Fill Non-Nature positive
      tidyr::replace_na(list(nffclass = 'Non-Nature positive')) %>% 
      tidyr::separate(scenario, into = c('f_continue', 'p_a', 'p_s', 'p_plant', 'f_cut', 'f_rotation', 'f_plant'), sep = '_') %>% 
      dplyr::mutate(f_continue = factor(f_continue),
                    p_a = factor(p_a), p_s = factor(p_s), p_plant = factor(p_plant),
                    f_cut = factor(f_cut), f_rotation = factor(f_rotation), f_plant = factor(f_plant)) %>% 
      dplyr::mutate(climate = climate.name, decade = tms)
    naniar::miss_var_summary(all.part.df)
    write_csv(all.part.df, file.path(output.dir, paste0('data_NFF-class_', climate.name, '_', tms, '.csv')))
    # Decide cp and minsplit parameters
    set.seed(2022)
    max.tree <- rpart(nffclass ~ f_continue + p_a + p_s + p_plant + f_cut + f_rotation + f_plant, 
                      data = all.part.df, method = 'class', parms = list(split='information'),
                      control = rpart.control(minsplit = min(table(all.part.df$nffclass))), 
                      cp = 0)
    printcp(max.tree)
    plotcp(max.tree)
    cps <- max.tree$cptable
    cp <- cps[which.min(cps[, 4] > cps[nrow(cps), 4] + cps[nrow(cps), 5]), 1]
    msplit <- cps[which.min(cps[, 4] > cps[nrow(cps), 4] + cps[nrow(cps), 5]), 2]
    # Apply final model
    set.seed(2022)
    np.part.result <- rpart(nffclass ~ f_continue + p_a + p_s + p_plant + f_cut + f_rotation + f_plant, 
                            data = all.part.df, method = 'class', parms = list(split='information'),
                            control = rpart.control(minsplit = msplit), cp = cp)
    np.part.result.party <- as.party(np.part.result)
    np.part.result.party$data$case <- fct_relevel(np.part.result.party$data$nffclass, case.list)
    layout.mod.df <- NULL
    if (climate.name == 'MRI_rcp26') {
      if (tms == 2030) {
        layout.mod.df <- data.frame(id = c(1),
                                    x = c(0.9),
                                    y = c(1))
      } else if (tms == 2050) {
        layout.mod.df <- data.frame(id = c(14),
                                    x = c(0.7),
                                    y = c(0.8))
      } else if (tms == 2090) {
        layout.mod.df <- data.frame(id = c(10),
                                    x = c(0.7),
                                    y = c(0.8))
      }
    } else {
      if (tms == 2030) {
        layout.mod.df <- data.frame(id = c(1),
                                    x = c(0.9),
                                    y = c(1))
      } else if (tms == 2090) {
        layout.mod.df <- data.frame(id = c(14),
                                    x = c(0.7),
                                    y = c(0.8))
      }
    }
    
    plt.part.all <- ggparty(np.part.result.party, terminal_space = 0.3, 
                            layout = layout.mod.df) + 
      geom_edge(size = 1.5) + 
      geom_edge_label(size = 3) +
      geom_node_plot(gglist = list(geom_bar(aes(x = '', fill = case), position = "fill"),
                                   xlab(NULL), ylab('% of cases for each node'),
                                   scale_fill_manual(values = case.col.vals, drop = TRUE),
                                   scale_x_discrete(expand = c(.0, .0)), scale_y_continuous(expand = c(.0, .0), labels = scales::percent),
                                   theme(text = element_text(size = 9), axis.line = element_line(color = 'black'),axis.ticks = element_line(color = 'black'))),
                     scales = "fixed", id = "terminal", shared_axis_labels = T, shared_legend = T) +
      geom_node_label(line_list = list(aes(label = paste("Node", id)), aes(label = splitvar)),
                      line_gpar = list(list(size = 8, col = "black", fontface = "bold"),
                                       list(size = 8)),
                      ids = "inner") +
      geom_node_label(aes(label = paste0("Node ", id, "\nN = ", nodesize)),
                      ids = "terminal", fontface = 'bold', size = 3, nudge_y = 0.017, nudge_x = 0.01) +
      theme(legend.position = "none") +
      labs(title = paste0('Climate: ', climate.name, ', Year: ', tms))
    # plt.part.all
    ggsave(file.path(output.dir, paste0('fig_supplement_decision-tree_', climate.name, '_', tms, '.pdf')), 
           plt.part.all, width = 20, height = 10)
    # Feature importance
    v.importance.df <- data.frame(varname = names(np.part.result$variable.importance),
                                  value = np.part.result$variable.importance) %>% 
      dplyr::mutate(varname = fct_relevel(varname, names(np.part.result$variable.importance)[order(np.part.result$variable.importance, decreasing = T)]))
    row.names(v.importance.df) <- 1:nrow(v.importance.df)
    (plt.vimportance <- ggplot(v.importance.df, aes(x = varname, y = value)) +
        geom_bar(stat = 'identity', fill = 'black') +
        scale_x_discrete(expand = c(.1, .1)) +
        scale_y_continuous(expand = c(.0, .01)) +
        labs(x = 'Variables', y = 'Importance', 
             title = 'Feature importance of decision tree', subtitle = paste0('Climate: ', climate.name, ', Year: ', tms)) +
        theme(panel.border = element_rect(color = 'grey', fill = NA),
              text = element_text(size = 16)))
    ggsave(file.path(output.dir, paste0('fig_supplement_decision-tree_feature-importance_', climate.name, '_', tms, '.pdf')), 
           plt.vimportance, width = 8, height = 6)
  }
}




# for journal writing ------
## Result -------
### Changes in scenario specific indicators --------
sr3.n <- bind_rows(read_csv(file.path(output.dir, 'data_top10-scenarios_MRI_rcp26.csv')) %>% 
                           mutate(climate = 'RCP2.6'),
                         read_csv(file.path(output.dir, 'data_top10-scenarios_MRI_rcp85.csv')) %>% 
                           mutate(climate = 'RCP8.5')) %>% 
  group_by(climate, idxtype, nplabel) %>% 
  summarise(n = n(), .groups = 'drop')

BindScenarios <- function(df) {
  paste(df$scenario, collapse = ' ')
}

sr3.cases <- bind_rows(read_csv(file.path(output.dir, 'data_top10-scenarios_MRI_rcp26.csv')) %>% 
                         mutate(climate = 'RCP2.6'),
                       read_csv(file.path(output.dir, 'data_top10-scenarios_MRI_rcp85.csv')) %>% 
                         mutate(climate = 'RCP8.5')) %>% 
  filter(nplabel != 'Under 90%') %>% 
  group_nest(climate, idxtype, nplabel) %>% 
  mutate(cases = map(data, BindScenarios)) %>% 
  select(-data) %>% unnest(cases)

sr3.summary <- left_join(sr3.n, sr3.cases)
write_csv(sr3.summary, file.path(output.dir, 'data_top10_summary.csv'))


### Changes in Pareto solutions ----------
sr4.n <- bind_rows(read_csv(file.path(output.dir, 'data_pareto-solutions_MRI_rcp26.csv')) %>% 
                     mutate(climate = 'RCP2.6'),
                   read_csv(file.path(output.dir, 'data_pareto-solutions_MRI_rcp85.csv')) %>% 
                     mutate(climate = 'RCP8.5')) %>% 
  group_by(climate, cases, Time) %>% 
  summarise(n = n(), .groups = 'drop')
BindScenarios <- function(df) {
  paste(df$scenario, collapse = ';')
}
sr4.cases <- bind_rows(read_csv(file.path(output.dir, 'data_pareto-solutions_MRI_rcp26.csv')) %>% 
                         mutate(climate = 'RCP2.6'),
                       read_csv(file.path(output.dir, 'data_pareto-solutions_MRI_rcp85.csv')) %>% 
                         mutate(climate = 'RCP8.5')) %>% 
  group_nest(climate, cases, Time) %>% 
  mutate(scenarios = map(data, BindScenarios)) %>% 
  select(-data) %>% unnest(scenarios)
sr4.cases
sr4.summary <- left_join(sr4.n, sr4.cases)
write_csv(sr4.summary, file.path(output.dir, 'data_pareto-solutions_summary.csv'))

### NFF case df --------
sr5.all.df <- list.files(path = output.dir, pattern = 'data_NFF-class_', full.names = T) %>% 
  map(read_csv) %>% 
  bind_rows()
sr5.case.list <- case.list[case.list %in% unique(sr5.all.df$nffclass)]
sr5.case.col.vals <- case.col.vals[names(case.col.vals) %in% sr5.case.list]
sr5.df <- sr5.all.df %>% 
  mutate(nffclass = factor(nffclass, levels = sr5.case.list)) %>% 
  filter(nffclass != 'Non-Nature positive') %>% 
  pivot_wider(names_from = 'decade', values_from = 'nffclass') %>% 
  group_by(climate, `2030`, `2050`, `2090`) %>% 
  count()
for (climate.name in climate.name.list) {
  plt_transition <- sr5.df %>% 
    filter(climate == climate.name) %>% 
    ggplot(aes(axis1 = `2030`,
             axis2 = `2050`,
             axis3 = `2090`,
             y = n)) +
    geom_alluvium(aes(fill = after_stat(stratum))) +
    geom_stratum(aes(fill = after_stat(stratum)), color = NA) +
    scale_x_discrete(limits = c("2030", "2050", "2090"),
                     expand = c(.1, .1)) +
    scale_y_continuous(expand = c(.01, .01)) +
    scale_fill_manual(values = sr5.case.col.vals, drop = T) +
    labs(title = "Transition of NFF class",
         subtitle = climate.name,
         x = 'Year', y = 'N. of cases',
         fill = 'NFF classification') +
    theme(text = element_text(size = 12))
    ggsave(file.path(output.dir, paste0('fig_supplement_NFF-class-transition', climate.name, '.pdf')), 
           plt_transition, width = 8, height = 6)
}

### Compute Class centroid -----
pareto.summary.dfs <- list(); pareto.all.summary.dfs <- list(); i <- 1
for (climate.name in climate.name.list) {
  for (t in c(2030, 2050, 2090)) {
    pareto3d.df <- read_csv(file.path(output.dir, paste0('data_pareto-3d_', climate.name, '.csv'))) %>% 
      filter(decades == t) %>% 
      right_join(sr5.all.df %>% 
                   replace_na(list(f_cut = '', f_rotation = '', f_plant = '')) %>% 
                   dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>%
                   dplyr::select(scenario, decade, climate, nffclass) %>% 
                   dplyr::filter(climate == climate.name, decade == t) %>% 
                   filter(!(nffclass %in% c('Non-Nature positive', 'Dominated solution'))),
                 by = c('scenario', 'decades' = 'decade'))
    (plt.pareto.nff.3d <- plot_ly() %>% 
        add_markers(data = pareto3d.df,
                    x = ~ns, y = ~nn, z = ~nc, type = 'scatter3d',
                    color = ~nffclass,
                    colors = case.col.vals, text = ~scenario,
                    hovertemplate = 'scenario: %{text}') %>%
        layout(scene = list(xaxis = list(title = 'Nature for Society'),
                            yaxis = list(title = 'Nature for Nature'),
                            zaxis = list(title = 'Nature as Culture'))))
    htmlwidgets::saveWidget(as_widget(plt.pareto.nff.3d), 
                            file.path(output.dir, paste0('res5b_nff_pareto_3d_', climate.name, '_', t, '.html')))
    cat('\n', climate.name, '-', t, '\n')
    pareto3d.dist <- pareto3d.df %>% 
      filter(nffclass %in% c('NC', 'NN', 'NS')) %>% 
      select(nffclass, nn, ns, nc) %>% 
      dist()
    print(pareto3d.dist)
    # Summary
    pareto.summary.dfs[[i]] <- pareto3d.df %>% 
      group_by(nffclass) %>% 
      summarise(N = n(),
                NN.mean = mean(nn, na.rm = T), NN.sd = sd(nn, na.rm = T),
                NS.mean = mean(ns, na.rm = T), NS.sd = sd(ns, na.rm = T),
                NC.mean = mean(nc, na.rm = T), NC.sd = sd(nc, na.rm = T)) %>% 
      mutate(climate = climate.name, decade = t)
    pareto.all.summary.dfs[[i]] <- pareto3d.df %>% 
      summarise(N = n(),
                NN.mean = mean(nn, na.rm = T), NN.sd = sd(nn, na.rm = T),
                NS.mean = mean(ns, na.rm = T), NS.sd = sd(ns, na.rm = T),
                NC.mean = mean(nc, na.rm = T), NC.sd = sd(nc, na.rm = T)) %>% 
      mutate(climate = climate.name, decade = t)
    i <- i + 1
  }
}
pareto.summary.df <- bind_rows(pareto.summary.dfs) %>% 
  mutate(NN = if_else(is.na(NN.sd), paste(round(NN.mean, 2)), paste(round(NN.mean, 2), "±", round(NN.sd, 2))),
         NS = if_else(is.na(NS.sd), paste(round(NS.mean, 2)), paste(round(NS.mean, 2), "±", round(NS.sd, 2))),
         NC = if_else(is.na(NC.sd), paste(round(NC.mean, 2)), paste(round(NC.mean, 2), "±", round(NC.sd, 2)))) %>% 
  select(climate, decade, nffclass, N, NN, NS, NC)

# Centroid
pareto.summary.wide.df <- bind_rows(pareto.summary.dfs) %>% 
  mutate(coord = paste0('(', round(NN.mean, 2), ', ', round(NS.mean, 2), ', ', round(NC.mean, 2), ')')) %>% 
  select(climate, decade, nffclass, coord) %>% 
  pivot_wider(names_from = decade, values_from = coord)
pareto.summary.wide.df
write_tsv(pareto.summary.wide.df, file.path(output.dir, 'data_pareto-class-center.tsv'))
# N
pareto.summary.wide.n.df <- bind_rows(pareto.summary.dfs) %>% 
  select(climate, decade, nffclass, N) %>% 
  pivot_wider(names_from = decade, values_from = N)
pareto.summary.wide.n.df
write_tsv(pareto.summary.wide.df, file.path(output.dir, 'data_pareto-class-center.tsv'))

# Overall mean
pareto.all.summary.wide.df <- bind_rows(pareto.all.summary.dfs) %>% 
  mutate(coord = paste0('(', round(NN.mean, 2), ', ', round(NS.mean, 2), ', ', round(NC.mean, 2), ')')) %>% 
  select(climate, decade, coord) %>% 
  pivot_wider(names_from = decade, values_from = coord)
pareto.all.summary.wide.df


## Exploratory -------------------
for (climate.name in climate.name.list) {
  nffclass.integ.df <- sr5.all.df %>% 
    replace_na(list(f_cut = '', f_rotation = '', f_plant = '')) %>% 
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>%
    dplyr::select(scenario, decade, climate, nffclass) %>% 
    tidyr::pivot_wider(names_from = decade, values_from = nffclass) %>% 
    dplyr::mutate(nffintegclass = paste(`2030`, `2050`, `2090`, sep = ' -> ')) %>% 
    select(climate, scenario, nffintegclass) %>% 
    dplyr::filter(climate == climate.name)
  unique.class <- rev(sort(unique(nffclass.integ.df$nffintegclass)))
  unique.class <- unique.class[!(unique.class %in% c('Non-Nature positive -> Non-Nature positive -> Non-Nature positive', 'Dominated solution -> Dominated solution -> Dominated solution'))]
  dominated.class <- unique.class[str_detect(unique.class, 'Dominated')]
  nondominated.class <- unique.class[!str_detect(unique.class, 'Dominated')]
  class.colors <- scales::gradient_n_pal(scales::dichromat_pal("BluetoOrange.10")(10))(seq(0, 1, length.out = length(unique.class)))
  names(class.colors) <- c(nondominated.class, dominated.class)

  nff.df <- read_csv(file.path(output.dir, paste0('data_nature-positive-scenarios_', climate.name, '_with-indices.csv'))) %>% 
    replace_na(list(f_cut = '', f_rotation = '', f_plant = '')) %>% 
    dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>%
    dplyr::select(scenario, Time, val, data.type, idxtype, varname.disp) %>% 
    dplyr::left_join(filter(nffclass.integ.df, climate == climate.name), by = 'scenario') %>% 
    dplyr::filter(data.type == 'Integrated Indices') %>% 
    dplyr::filter(!(nffintegclass %in% c('Dominated solution -> Dominated solution -> Dominated solution', 'Non-Nature positive -> Non-Nature positive -> Non-Nature positive'))) %>% 
    dplyr::mutate(isdominant = if_else(str_detect(nffintegclass, 'Dominated'), 'Including dominated solutions', 'All Pareto solutions'),
                  idxtype = case_when(idxtype == 'common' ~ 'Common',
                                      idxtype == 'nn' ~ 'Nature for Nature',
                                      idxtype == 'nc' ~ 'Nature as Culture',
                                      idxtype == 'ns' ~ 'Nature for Society')) %>% 
    dplyr::mutate(isdominant = if_else(nffintegclass == 'NN-NS Pareto -> Dominated solution -> Dominated solution', 'NN-NS Pareto -> Dominated', isdominant)) %>% 
    dplyr::mutate(idxtype = fct_relevel(idxtype, 'Common', 'Nature for Nature', 'Nature for Society', 'Nature as Culture'))

  nff.df %>% 
    filter(idxtype == 'Common', Time == 2) %>% 
    group_by(nffintegclass) %>% 
    summarise(n = n())
  plt.nffalternatives <- nff.df %>% 
    ggplot() +
    annotate("rect", xmin = 2030, xmax = 2039, ymin = -Inf, ymax = Inf, alpha = 0.2, fill="white") +
    annotate("rect", xmin = 2050, xmax = 2059, ymin = -Inf, ymax = Inf, alpha = 0.2, fill="white") +
    annotate("rect", xmin = 2091, xmax = 2100, ymin = -Inf, ymax = Inf, alpha = 0.2, fill="white") +
    geom_line(aes(x = 2015 + Time, y = val, 
                  group = interaction(nffintegclass, data.type, idxtype, scenario),
                  color = nffintegclass),
              size = .8, alpha = .8) +
    scale_color_manual(values = class.colors) +
    scale_x_continuous(expand = c(.01, .01), breaks = seq(2020, 2100, by = 10), labels = seq(2020, 2100, by = 10)) +
    scale_y_continuous(expand = c(.01, .01), limits = c(0, 1)) +
    labs(x = 'Year', y = 'Min-max scaled scores', title = 'Integrated indices', subtitle = climate.name,
         color = NULL) +
    guides(col = guide_legend(nrow = 7, byrow = FALSE)) +
    theme(panel.border = element_rect(color = 'grey', fill = NA),
          text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = 'bottom', legend.key = element_rect(fill = NA, color = NA), legend.background = element_rect(fill = 'grey10'),
          legend.text = element_text(size = 8, color = 'white'),
          panel.background = element_rect(fill= 'grey10')) +
    # facet_wrap(~idxtype, scales = 'free')
    facet_grid(idxtype~isdominant, scales = 'free')
  ggsave(file.path(output.dir, paste0('res_nff_alternatives_', climate.name, '.pdf')), 
         plt.nffalternatives, width = 9, height = 12)
}


data.integ.top10 <- data.integ.indices %>% 
  dplyr::mutate(scenario = paste(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, sep = '_')) %>% 
  dplyr::left_join(flag.df, by = c('scenario', 'idxtype')) %>% 
  dplyr::select(-scenario) %>% 
  dplyr::mutate(idxtype = case_when(idxtype == 'nn' ~ 'Nature for Nature',
                                    idxtype == 'ns' ~ 'Nature for Society',
                                    idxtype == 'nc' ~ 'Nature as Culture',
                                    TRUE ~ as.character(NA)),
                idxtype = fct_relevel(idxtype, 'Nature for Nature', 'Nature for Society', 'Nature as Culture')) %>% 
  dplyr::filter(!is.na(idxtype))
(plt.res2.main <- ggplot() +
    annotate("rect", xmin = 2030, xmax = 2039, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
    annotate("rect", xmin = 2050, xmax = 2059, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
    annotate("rect", xmin = 2091, xmax = 2100, ymin = -Inf, ymax = Inf, alpha = 0.1, fill="deepskyblue") +
    geom_line(data = filter(data.integ.top10, nplabel == 'Under 90%'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    geom_line(data = filter(data.integ.top10, nplabel == 'BaU'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    geom_line(data = filter(data.integ.top10, nplabel == '2030s and 2050s'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    geom_line(data = filter(data.integ.top10, nplabel == '2050s and 2090s'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    geom_line(data = filter(data.integ.top10, nplabel == '2030s and 2090s'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    geom_line(data = filter(data.integ.top10, nplabel == '2030s'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    geom_line(data = filter(data.integ.top10, nplabel == '2050s'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    geom_line(data = filter(data.integ.top10, nplabel == '2090s'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    geom_line(data = filter(data.integ.top10, nplabel == '2030s, 2050s, and 2090s'), 
              aes(x = Time + 2015, y = val.integ, group = interaction(f_continue, p_a, p_s, p_plant, f_cut, f_rotation, f_plant, climate, nplabel), color = nplabel, size = nplabel)) +
    scale_color_manual(values = iccolors) +
    scale_size_manual(values = icsizes) +
    scale_x_continuous(expand = c(.01, .01), breaks = seq(2020, 2100, by = 10), labels = seq(2020, 2100, by = 10)) +
    scale_y_continuous(expand = c(.01, .01), limits = c(0, 1)) +
    labs(x = 'Year', y = 'Min-max scaled scores', title = 'Integrated NFF specific indices', subtitle = climate.name,
         caption = 'Colored lines show the top 10% of nature positive cases for each decade.',
         color = 'Top 10%', size = 'Top 10%') +
    facet_wrap(~idxtype, ncol = 2, scales = 'free') +
    theme(panel.border = element_rect(color = 'grey', fill = NA),
          text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(.8, .2)))
ggsave(file.path(output.dir, paste0('res2_specific_indicators_', climate.name, '_sub.pdf')), plt.res2.sub, width = 16, height = 16)
ggsave(file.path(output.dir, paste0('res2_specific_indicators_', climate.name, '_main.pdf')), plt.res2.main, width = 8, height = 8)
