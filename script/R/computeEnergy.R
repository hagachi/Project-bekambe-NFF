# title:  computeEnergy program 
#         (a??Ne3aa??a??a?aa??a?1a??cc??e???0a?Ra1?La???a3?Na??)
# auther: marimi (original) & Hagachi (modified)
# date:   2019/10/26


# FUNCTION ***********************************************************************************
CalcBiomEnergy <- function(scenario.dir, scenario, climate, times) {
  MOIST_CONTENT <- 10 # %
  YIELD <- 80 # %
  LHV <- 16 # Gj / t (Gj / Mg)
  harvest.db <- readr::read_csv(file.path(scenario.dir, 'biomass-harvest-summary-log.csv'))
  biom_energy.df <- harvest.db %>%
    dplyr::filter(Prescription != "Agriculture")%>%
    dplyr::filter(Prescription != "stop_and_solar") %>%
    dplyr::filter(str_detect(Prescription, pattern = paste(c('thinning', 'abandoned'), collapse = '|'))) %>% # 放棄地と間伐材を使う
    # dplyr::filter(str_detect(Prescription, pattern = 'abandoned')) %>% # 放棄地のみを使う
    dplyr::filter(Time %in% times) %>% 
    dplyr::group_by(Time) %>%
    dplyr::summarise(biom = sum(TotalBiomassHarvested)) %>%  # biom (Mg) 
    dplyr::ungroup() %>%
    # tms.lista???a??a??a??ea??a?ca??e?N?cR?a?C-a???a??a\-a??fa??a?|a??a???a??NAa???a?aa??a1?La?Rbioma??fOa???a??a??
    dplyr::full_join(data.frame(tms = times), by = c('Time' = 'tms')) %>%
    dplyr::mutate(biom = if_else(is.na(biom), 0, biom)) %>%
    # oven dry Mg-biom * (consider moisture content, air dry Mg-biom) * (loss) * (lower heating value)
    dplyr::mutate(biom_energy = biom * (100 / (100 - MOIST_CONTENT)) * (YIELD / 100) * LHV * 10^-3,  # Gj-->Tj
                  scenario = scenario,
                  climate = climate)
  return(biom_energy.df)
}


# MAIN process =============================================================================================
# biomass energy =========================================================
biomass.list <- list()
i <- 1
for (scenario in scenario.name.list) {
  if (stringr::str_detect(scenario, 'noManagement')) next
  for (climate in climate.name.list) {
    scenario.dir <- paste0(root.dir, "/", scenario, "/", climate)
    print(scenario.dir)
    # biomass.list[[i]] <- CalcBiomEnergy(scenario.dir, scenario, climate, 1:85)
    biomass.list[[i]] <- CalcBiomEnergy(scenario.dir, scenario, climate, tms.list)
    i <- i + 1
  }
}
biomass.df <- dplyr::bind_rows(biomass.list)

setwd(energy.dir)
# without.BaU.biomass <- read.csv("biomass.csv")
# with.BaU.biomass <- rbind(without.BaU.biomass, biomass.df)
# write_csv(with.BaU.biomass, file.path(energy.dir, 'biomass.csv'))
write_csv(biomass.df, file.path(energy.dir, 'biomass.csv'))


# Visualize
ggplot(biomass.df, aes(x = Time + 2015, y = biom_energy, group = scenario, color = climate)) +
  ggthemes::scale_color_colorblind() +
  geom_line()



# solar power energy ===============================
SOL_UNIT <- 61.58 # kWh/m2/yr
CONV_UNIT <- 3.6 * 10^6 * 10^-12 # TJ/kWh
solar.list <- list()
i <- 1
for (scenario in scenario.name.list) {
  for (climate in climate.name.list) {
    print(scenario)
    for (tms in tms.list) {
    # for (tms in 1:85) {
      dominant.ras <- raster::raster(paste0(lulc.dir, "/dominantSpp-", scenario, "_", climate, "_", tms, ".tif"))
      isSolar.ras <- dominant.ras == 21
      energy.sol <- cellStats(isSolar.ras, sum) * SOL_UNIT * CONV_UNIT * 10^4 # TJ
      solar.list[[i]] <- data.frame(Time = tms,
                                    solar_energy = energy.sol,
                                    scenario = scenario,
                                    climate = climate)
      i <- i + 1
    }
  }
}
solar.df <- bind_rows(solar.list)

setwd(energy.dir)
# without.BaU.solar <- read.csv("solar.csv")
# with.BaU.solar <- rbind(without.BaU.solar, solar.df)
# write_csv(with.BaU.solar, file.path(energy.dir, 'solar.csv'))
write_csv(solar.df, file.path(energy.dir, 'solar.csv'))

# Visualize
ggplot(solar.df, aes(x = Time + 2015, y = solar_energy, color = scenario)) +
  ggthemes::scale_color_colorblind() +
  geom_line()



# plot total expected energy amount ====================================== 
# (biomass energy + solar power energy) by one line (solar power scenario) 
total.energy.df <- dplyr::full_join(solar.df, biomass.df, 
                                    by = c("Time" = "Time",
                                           "scenario" = "scenario",
                                           "climate" = "climate"))
total.energy.df[is.na(total.energy.df)] <- 0
total.energy.df <- total.energy.df %>%
  mutate(total_energy = solar_energy + biom_energy)

setwd(energy.dir)
# without.BaU.total.energy <- read.csv("total_energy.csv")
# with.BaU.total.energy <- rbind(without.BaU.total.energy, total.energy.df)
# write_csv(with.BaU.total.energy, file.path(energy.dir, 'total_energy.csv')) # Tj
write_csv(total.energy.df, file.path(energy.dir, 'total_energy.csv')) # Tj



# plot energy by scenarios *******************************************************************
plt <- ggplot()+
  geom_line(data = total.energy.df, 
            aes(x = Time + 2015,  
                y = total_energy, 
                group = interaction(scenario,climate),
                color = climate))+ 
  # viridis::scale_color_viridis(discrete = TRUE) +
  ggthemes::scale_color_colorblind() +
  xlab("Year")+
  ylab("Energy (TJ)") +
  ggtitle('The amount of potential total energy') +
  theme_bw()
ggsave(file.path(energy.dir, 'total_energy.png'), plt)

