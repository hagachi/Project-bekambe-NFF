# memo: prescriptionID 0-27 case

#' # Define functions
# identify if the forest is natural or not.
compute_forest_type <- function(dat_stack) {
  
  forest_type_scores <- list()
  forest_type_id <- 0
  max_score <- 0
  
  # 牧草地と放棄地、人工林を除く
  if(dat_stack[["lulc_ras"]] == 8){ # isActivePasture
    return(10)
  
  } else if (dat_stack[['lulc_ras']] == 31 | # isPotentialWetland
             dat_stack[['lulc_ras']] == 21 | # isSolarPowerPlant
             dat_stack[['lulc_ras']] == 41 | # grassland after abandonment
             dat_stack[['lulc_ras']] == 42 ) { # grassland after clearcut
    return(0)
    
  } else {
    # Calculate scores for each forest type
    # IDの順は、
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
    
    # 植生自然度 > 7で天然林のクラス
    if (dat_stack[["is_artificial"]] == 1) {  
      forest_type_scores[[1]] <- 0
      forest_type_scores[[2]] <- 0
      forest_type_scores[[8]] <- dat_stack[['abiesach']]
      forest_type_scores[[9]] <- dat_stack[['larikaem']]
    } else {
      if (dat_stack[["is_artificial_initial"]] == 1 &
          dat_stack[["is_natural"]] == 0) {
        forest_type_scores[[1]] <- 0
        forest_type_scores[[2]] <- 0
        forest_type_scores[[8]] <- dat_stack[['abiesach']]
        forest_type_scores[[9]] <- dat_stack[['larikaem']]
      } else if (dat_stack[["is_artificial_initial"]] == 1 &
                 dat_stack[["is_natural"]] == 1){
        forest_type_scores[[1]] <- dat_stack[['abiesach']] + dat_stack[['quercris']]
        forest_type_scores[[2]] <- dat_stack[['abiesach']]
        forest_type_scores[[8]] <- 0
        forest_type_scores[[9]] <- 0
      } else {
        forest_type_scores[[1]] <- dat_stack[['abiesach']] + dat_stack[['quercris']]
        forest_type_scores[[2]] <- dat_stack[['abiesach']]
        forest_type_scores[[8]] <- 0
        forest_type_scores[[9]] <- 0
      }
    }
    # forest_type_scores[[2]] <- dat_stack[['abiesach']]
    forest_type_scores[[3]] <- dat_stack[['ulmudavi']]
    forest_type_scores[[4]] <- dat_stack[['alnujapo']] + dat_stack[['fraxmand']]
    forest_type_scores[[5]] <- dat_stack[['alnujapo']]
    # 以下、植生自然度 <= 7
    forest_type_scores[[6]] <- dat_stack[['betuplat']] + dat_stack[['quercris']]
    forest_type_scores[[7]] <- dat_stack[['sasagras']]
    forest_type_scores[[10]] <- dat_stack[['pastgras']]
    
    # Identify which forest type is the best
    for (type_id in 1:length(forest_type_scores)) {
      if (forest_type_scores[[type_id]] > max_score) {
        max_score <- forest_type_scores[[type_id]]
        forest_type_id <- type_id
      }
    }
    
    return(forest_type_id)
  }
}




fill_zero_forest_type <- function(fill_stack) {
  
  if (fill_stack[['landis']] == 0) {
    return(fill_stack[['initial']])
  } else {
    return(fill_stack[['landis']])
  }
}


identify_artificial_forest <- function(dat_brick) {
  clearcut_ids <- c(2,3,5,7:9) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!clearcutting
  is_harvested_brick <- dat_brick %in% clearcut_ids
  is_harvested_brick[is_harvested_brick == 0] <- NA
  is_harvested <- merge(is_harvested_brick)
  is_harvested_brick[is.na(is_harvested_brick)] <- 0
  return(is_harvested)
}

identify_natural_forest <- function(dat_brick) { # as secondary forest
  clearcut_ids <- c(10:24) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!selective cutting
  is_harvested_brick <- dat_brick %in% clearcut_ids
  is_harvested_brick[is_harvested_brick == 0] <- NA
  is_harvested <- merge(is_harvested_brick)
  is_harvested_brick[is.na(is_harvested_brick)] <- 0
  return(is_harvested)
}
