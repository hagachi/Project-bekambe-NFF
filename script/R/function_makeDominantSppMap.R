# ***********************************
# Function: detect dominant species
# ***********************************
# memo: prescriptionID 0-27 case


# DetectGrasslands function
DetectGrasslands <- function(x) {
  abandoned_ids <- c(28) + 1
  clearcut_ids <- c(5, 6, 8, 10, 11, 12, 30, 31) + 1
  lutype <- 0
  
  # 樹高が5m(ヤチダモ・ミズナラ・ハルニレは6m)になるまでは草地扱い
  # 針葉樹は
  # トドマツは北海道トドマツ人工林収穫予測ソフトのデータをそのまま
  # カラマツはカラマツ人工林収穫予測ソフトよりExcelのSolverで外挿
  # 広葉樹は森林計画照査情報処理要領より
  # トドマツ・ヤチダモ・ミズナラ・ハルニレはデータをそのまま
  # シラカバ・ハンノキはExcelのSolverで外挿
  # 全樹種ともバイオマスのチューニングと同じものを使う
  # カラマツlarikaem：　　6年間
  # トドマツabiesach：　　13年間
  # シラカンバbetuplat：　５年間
  # ハンノキalnujapo：　　８年間
  # ヤチダモfraxmand・ミズナラquercris・ハルニレulmudavi：　１４年間
  # "01abiesach", "02larikaem", "03betuplat", "04ulmudavi", "05alnujapo", "06quercris", "07fraxmand", "08pastgras", "09sasagras"
  
  abie.years <- 13
  lari.years <- 6
  betu.years <- 5
  alnu.years <- 8
  other.bro.years <- 14
  other.bro.spp <- c(4, 6, 7)
  
  if (x[[1]] == 1) {
    y <- abie.years
  } else if (x[[1]] == 2) {
    y <- lari.years
  } else if (x[[1]] == 3) {
    y <- betu.years
  } else if (x[[1]] == 5) {
    y <- alnu.years
  }  else if (x[[1]] %in% other.bro.spp) {
    y <- other.bro.years
  } else {
    return(lutype)
  }

  # 1. identify abandoned pastureland
  z <- max((length(x)-y+1),3)
  for (lyr in z:length(x)) {
    # 1年前には管理をしていて，次の年に管理をやめたら放棄地
    if (x[[lyr-1]] %in% abandoned_ids && !(x[[lyr]] %in% abandoned_ids)){
      lutype <- 1
    }
  }

  # 2. identify grassland after clearcut
  for (lyr in z:length(x)) {
    if (x[[lyr]] %in% clearcut_ids){
      lutype <- 2
    }
  }
  return(lutype)
}


# x: raster stack of each species biomass
DetectDominantSpp <- function(x) {

	# determin maximum value in each cell
	max_AGB <- max(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]], 
	               x[[8]], x[[9]])

	# detect dominant species code
	# 1-10のコードはspp_namesリストの並び順に等しい
	# "01abiesach", "02larikaem", "03betuplat", "04ulmudavi", "05alnujapo", "06quercris", "07fraxmand", "08pastgras", "09sasagras"
	
	
	if (x[[10]] == 28+1) {                         # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# 農地管理をしている = x[11] (つまりprescripts-yy.img)で
		# ２が格納されているセルでは、バイオマス量にかかわらず牧草地とする。
		dominant <- 8
	} else if (x[[10]] == 29+1) {             # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# solar power generation plant
		dominant <- 21
	# } else if (x[[12]] > 0) {
	#   dominant <- 40 + x[[12]] # 41: abandoned pastureland, 42: grass after clearcut
	} else {
		if (max_AGB == x[[1]]) {
			dominant <- 1
		} else if (max_AGB == x[[2]]) {
			dominant <- 2
		} else if (max_AGB == x[[3]]) {
			dominant <- 3
		} else if (max_AGB == x[[4]]) {
			dominant <- 4
		} else if (max_AGB == x[[5]]) {
			dominant <- 5
		} else if (max_AGB == x[[6]]) {
			dominant <- 6
		} else if (max_AGB == x[[7]]) {
			dominant <- 7
		} else if (max_AGB == x[[8]]) {
			dominant <- 8
		} else if (max_AGB == x[[9]]) {
			dominant <- 9
		}

    # if AGB = 0, it means there are no species (dominant = 0)
    if (max_AGB == 0){
        dominant <- 0
    }
	}

	# check if abandoned pastureland is expected to become wetland
	if (x[[10]] != 28+1 && x[[11]] == 1) {           # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		dominant <- 31
	}

  return(dominant)
}


MakeDominantSppMap <- function(root.dir, scenario.dir, scenario, tms, spp.name.list) {

	# read biomass of 10 species and prescript maps
	biomass.list <- paste0(scenario.dir, "/OutputMaps/biomass/", spp.name.list, "-", tms, ".tif")
	biomass.stack <- stack(biomass.list)
	prescript.name <- paste0(scenario.dir, "/OutputMaps/harvest/prescripts-", tms, ".img")
	prescript.ras <- raster::raster(prescript.name)
	wetland.name <- paste0(root.dir, "/data/bekanbe_wet_pasture.tif")
	wetland.ras <- raster::raster(wetland.name)
	
	# apply same spatial extent to make raster stack
	ext <- extent(biomass.stack)
	extent(prescript.ras) <- ext
	extent(wetland.ras) <- ext
	dat.stack <- stack(c(biomass.stack, prescript.ras, wetland.ras))
	dat.stack[is.na(dat.stack)] <- 0
	
	# ************************************
	# detect dominant species based on AGB and prescription
	# using "detectDominantSpp" function (SEE ABOVE)
	dominant.spp.ras <- calc(x = dat.stack, fun = DetectDominantSpp)
	# ************************************
	
	# detect grassland
	ini.lulc.ras <- raster(paste0(data.dir, "/bekanbe_DSI_LULC.tif"))
	ini.agri.ras <- (ini.lulc.ras==203)*(28+1) # 28+1 is Prescription code: Agriculture
	extent(ini.agri.ras) <- extent(biomass.stack)
	
	
	# 過去N-1年間の間に，耕作放棄・ClearCutがあったグリッドを特定
	N <- 14 + 1 # 放棄地は1年前を見て判別するので+1枚のprescription mapを読む．
	            # 森林が成立するまでが最大年数の樹種に合わせて14年
	if (tms > N){
	  years <- (tms-N):tms
	  prescripts.name <- paste0(scenario.dir, "/OutputMaps/harvest/prescripts-", years, ".img")
	  prescripts.stack <- raster::stack(prescripts.name)
	  isgrass.data.stack <- stack(dominant.spp.ras, prescripts.stack)
	} else {
	  years <- 1:tms
	  prescripts.name <- paste0(scenario.dir, "/OutputMaps/harvest/prescripts-", years, ".img")
	  prescripts.stack <- raster::stack(prescripts.name)
	  isgrass.data.stack <- stack(dominant.spp.ras, ini.agri.ras, prescripts.stack)
	}
	isgrass.data.stack[is.na(isgrass.data.stack)] <- 0
	isGrass.ras <- calc(x = isgrass.data.stack, fun = DetectGrasslands) #, forcefun = TRUE)
	
	grass.ras <- isGrass.ras + 40
	grass.ras[grass.ras == 40] <- NA
	dominant.grass.ras <- merge(grass.ras, dominant.spp.ras)
	
	return(list(dominant.spp.ras, dominant.grass.ras))
}

