# identify landuse type; openArea, broadLeafOrMixedForest, plantedForest.
compute_landuse_type <- function(dat_stack) {
  # dat_stack[1]:total_bio_ras
  # dat_stack[2]:broad_bio_ras
  # dat_stack[3]:domi_ras
  
  # lutypeのidを入れる変数を初期化
  lutype_id <- NA
  
  if(dat_stack[[3]] == 8 |    # 牧草地
     dat_stack[[3]] == 9 |    # ササ
     dat_stack[[3]] == 31 |   # 湿地
     dat_stack[[3]] == 41 |   # 放棄後の草地
     dat_stack[[3]] == 42){   # 施業後の草地
    # open area
    lutype_id <- 1
    
  } else if(dat_stack[[3]] == 21){   
    # パネル設置用地ではlutype_idは０のままにする。
    lutype_id <- 0
    
  } else {  
    if(dat_stack[[1]]>0){
      if(dat_stack[[2]] / dat_stack[[1]] > 0.3){    # 総バイオマス量に対する広葉樹のバイオマス量のラスタ
        # 広葉樹バイオマス/総バイオマス > 0.3
        # 広葉樹林　+　マツ混交林
        lutype_id <- 2 
        
      } else {
        # 広葉樹バイオマス/総バイオマス <= 0.3
        # 針葉樹植林
        lutype_id <- 3
      }
    }
  }
  return(lutype_id)
}
