library(dplyr)
library(rlang)
library(readr)

for (league in c("E1", "E2", "E3", "E4", "D1", "F1", "SP1", "I1")){
  
  all_data = list.files(path=paste("data2", league, sep="/"), full.names = TRUE) %>% lapply(read_csv) %>% bind_rows()
  all_data$Date = as.Date(all_data$Date, "%d/%m/%y")
  all_data = all_data[order(all_data$Date),]
  
  match_info = all_data[, c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "HTHG", "HTAG", "HS", "AS", "HST", "AST", "HC", "AC", "HF", "AF")]
  
  
  match_info = match_info[complete.cases(match_info), ]
  match_info$GT = match_info$FTHG + match_info$FTAG
  match_info$GD = match_info$FTHG - match_info$FTAG
  match_info$FR = sign(match_info$FTHG - match_info$FTAG)
  
  hist(match_info$GT)
  hist(match_info$GD)
  
  all_match_info = data.frame()
  pb = txtProgressBar(min = 0, max = nrow(match_info), initial = 0) 
  for (i in c(1:nrow(match_info))){
    this_home = match_info$HomeTeam[i]
    this_away = match_info$AwayTeam[i]
    this_part = match_info[1:(i-1),]
    this_part_home = this_part[this_part$HomeTeam == this_home | this_part$AwayTeam == this_home,]
    this_part_away = this_part[this_part$HomeTeam == this_away | this_part$AwayTeam == this_away,]
    if (nrow(this_part_home) < 5 | nrow(this_part_away) < 5){
      next
    }
    this_part_home = tail(this_part_home, n=5)
    this_part_away = tail(this_part_away, n=5)
    new_df_home = data.frame()
    new_df_away = data.frame()
    for (j in c(1:5)){
      if (this_part_home$HomeTeam[j] == this_home){
        this_row_home = data.frame("FG"=this_part_home$FTHG[j],
                                   "HG"=this_part_home$HTHG[j],
                                   "S"=this_part_home$HS[j],
                                   "ST"=this_part_home$HST[j],
                                   "C"=this_part_home$HC[j],
                                   "F"=this_part_home$HF[j],
                                   "FGC"=this_part_home$FTAG[j],
                                   "HGC"=this_part_home$HTAG[j],
                                   "SC"=this_part_home$AS[j],
                                   "STC"=this_part_home$AST[j],
                                   "CC"=this_part_home$AC[j],
                                   "FC"=this_part_home$AF[j])
      } else {
        this_row_home = data.frame("FG"=this_part_home$FTAG[j],
                                   "HG"=this_part_home$HTAG[j],
                                   "S"=this_part_home$AS[j],
                                   "ST"=this_part_home$AST[j],
                                   "C"=this_part_home$AC[j],
                                   "F"=this_part_home$AF[j],
                                   "FGC"=this_part_home$FTHG[j],
                                   "HGC"=this_part_home$HTHG[j],
                                   "SC"=this_part_home$HS[j],
                                   "STC"=this_part_home$HST[j],
                                   "CC"=this_part_home$HC[j],
                                   "FC"=this_part_home$HF[j])
      }
      new_df_home = rbind(new_df_home, this_row_home)
      
      
      if (this_part_away$HomeTeam[j] == this_away){
        this_row_away = data.frame("FG"=this_part_away$FTHG[j],
                                   "HG"=this_part_away$HTHG[j],
                                   "S"=this_part_away$HS[j],
                                   "ST"=this_part_away$HST[j],
                                   "C"=this_part_away$HC[j],
                                   "F"=this_part_away$HF[j],
                                   "FGC"=this_part_away$FTAG[j],
                                   "HGC"=this_part_away$HTAG[j],
                                   "SC"=this_part_away$AS[j],
                                   "STC"=this_part_away$AST[j],
                                   "CC"=this_part_away$AC[j],
                                   "FC"=this_part_away$AF[j])
      } else {
        this_row_away = data.frame("FG"=this_part_away$FTAG[j],
                                   "HG"=this_part_away$HTAG[j],
                                   "S"=this_part_away$AS[j],
                                   "ST"=this_part_away$AST[j],
                                   "C"=this_part_away$AC[j],
                                   "F"=this_part_away$AF[j],
                                   "FGC"=this_part_away$FTHG[j],
                                   "HGC"=this_part_away$HTHG[j],
                                   "SC"=this_part_away$HS[j],
                                   "STC"=this_part_away$HST[j],
                                   "CC"=this_part_away$HC[j],
                                   "FC"=this_part_away$HF[j])
      }
      new_df_away = rbind(new_df_away, this_row_away)
    }
    this_math_info = data.frame("HFGC" = mean(new_df_home$FGC),
                                "HFG" = mean(new_df_home$FG),
                                "HHGC" = mean(new_df_home$HGC),
                                "HHG" = mean(new_df_home$HG),
                                "HSC" = mean(new_df_home$SC),
                                "HS" = mean(new_df_home$S),
                                "HSTC" = mean(new_df_home$STC),
                                "HST" = mean(new_df_home$ST),
                                "HCC" = mean(new_df_home$CC),
                                "HC" = mean(new_df_home$C),
                                "HFC" = mean(new_df_home$FC),
                                "HF" = mean(new_df_home$F),
                                "AFGC" = mean(new_df_away$FGC),
                                "AFG" = mean(new_df_away$FG),
                                "AHGC" = mean(new_df_away$HGC),
                                "AHG" = mean(new_df_away$HG),
                                "ASC" = mean(new_df_away$SC),
                                "AS" = mean(new_df_away$S),
                                "ASTC" = mean(new_df_away$STC),
                                "AST" = mean(new_df_away$ST),
                                "ACC" = mean(new_df_away$CC),
                                "AC" = mean(new_df_away$C),
                                "AFC" = mean(new_df_away$FC),
                                "AF" = mean(new_df_away$F),
                                "FR" = match_info$FR[i],
                                "GT" = match_info$GT[i],
                                "GD" = match_info$GD[i])
    
    all_match_info = rbind(all_match_info, this_math_info)
    setTxtProgressBar(pb,i)
  }
  close(pb)
  print(table(all_match_info$FR))
  spec = c(train = .7, test = .15, validate = .15)
  
  g = sample(cut(
    seq(nrow(all_match_info)), 
    nrow(all_match_info)*cumsum(c(0,spec)),
    labels = names(spec)
  ))
  
  res = split(all_match_info, g)
  
  train = res$train
  valid = res$valid
  test = res$test
  
  train_normalized = as.data.frame(scale(as.matrix(train[,c(1:(ncol(train)-3))])))
  valid_normalized = as.data.frame(scale(valid[,c(1:(ncol(valid)-3))]))
  test_normalized = as.data.frame(scale(test[,c(1:(ncol(test)-3))]))
  
  train_targets = train[,c((ncol(train)-2):ncol(train))]
  valid_targets = valid[,c((ncol(valid)-2):ncol(valid))]
  test_targets = test[,c((ncol(test)-2):ncol(test))]
  
  # save(train_normalized,file=paste("datalast", league, "train_features.Rda", sep="/"))
  # save(valid_normalized,file=paste("datalast", league, "valid_features.Rda", sep="/"))
  # save(test_normalized,file=paste("datalast", league, "test_features.Rda", sep="/"))
  # 
  # save(train_targets,file=paste("datalast", league, "train_targets.Rda", sep="/"))
  # save(valid_targets,file=paste("datalast", league, "valid_targets.Rda", sep="/"))
  # save(test_targets,file=paste("datalast", league, "test_targets.Rda", sep="/"))
  
}


# leagues = c("E0", "E1", "E2", "E3", "D1", "F1", "I1", "SP1")
# train_all_data = data.frame()
# train_all_targets = data.frame()
# valid_all_data = data.frame()
# valid_all_targets = data.frame()
# test_all_data = data.frame()
# test_all_targets = data.frame()
# for (league in leagues){
#   print(league)
#   load(paste("datalast", league, "train_features.Rda", sep="/"))
#   train_all_data = rbind(train_all_data, train_normalized)
#   load(paste("datalast", league, "train_targets.Rda", sep="/"))
#   train_all_targets = rbind(train_all_targets, train_targets)
#   
#   load(paste("datalast", league, "valid_features.Rda", sep="/"))
#   valid_all_data = rbind(valid_all_data, valid_normalized)
#   load(paste("datalast", league, "valid_targets.Rda", sep="/"))
#   valid_all_targets = rbind(valid_all_targets, valid_targets)
#   
#   load(paste("datalast", league, "test_features.Rda", sep="/"))
#   test_all_data = rbind(test_all_data, test_normalized)
#   load(paste("datalast", league, "test_targets.Rda", sep="/"))
#   test_all_targets = rbind(test_all_targets, test_targets)
# }


# save(train_all_data,file=paste("datalast", "train_all_data.Rda", sep="/"))
# save(valid_all_data,file=paste("datalast", "valid_all_data.Rda", sep="/"))
# save(test_all_data,file=paste("datalast", "test_all_data.Rda", sep="/"))
# 
# save(train_all_targets,file=paste("datalast", "train_all_targets.Rda", sep="/"))
# save(valid_all_targets,file=paste("datalast", "valid_all_targets.Rda", sep="/"))
# save(test_all_targets,file=paste("datalast", "test_all_targets.Rda", sep="/"))


if (0){
  RF1 = rbind(train_all_data, valid_all_data, test_all_data)
  RF2 = rbind(train_all_targets, valid_all_targets, test_all_targets)
  RF3 = cbind(RF1, RF2)
  RF3 = RF3[sample(1:nrow(RF3)), ]
  RF4 = RF3[,1:24]
  RF5 = RF3[,25:35]
  
  train_all_targets = RF5[1:dim(train_all_data)[1],]
  valid_all_targets = RF5[(dim(train_all_data)[1]+1):(dim(train_all_data)[1] + dim(valid_all_data)[1]),]
  test_all_targets = RF5[(dim(train_all_data)[1] + dim(valid_all_data)[1]):((dim(train_all_data)[1] + dim(valid_all_data)[1] + dim(test_all_data)[1])),]
  
  
  
  train_all_data = RF4[1:dim(train_all_data)[1],]
  valid_all_data = RF4[(dim(train_all_data)[1]+1):(dim(train_all_data)[1] + dim(valid_all_data)[1]),]
  test_all_data = RF4[(dim(train_all_data)[1] + dim(valid_all_data)[1]):((dim(train_all_data)[1] + dim(valid_all_data)[1] + dim(test_all_data)[1])),]
  
}
