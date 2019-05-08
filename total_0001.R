# # setwd
# setwd("/Users/evanthompson/SDP/epa")
# 
# 
# 
# # load packages
# library(nflscrapR)
# library(lubridate)
# library(stringr)
# library(tidyr)
# library(tidyverse)
# library(devtools)
# library(dplyr)
# library(DescTools)
# library(tictoc)
# 
# 
# 
# # # load 2018 play-by-play data
# season2009 <- season_play_by_play(Season = 2009)
# season2010 <- season_play_by_play(Season = 2010)
# season2011 <- season_play_by_play(Season = 2011)
# season2012 <- season_play_by_play(Season = 2012)
# season2013 <- season_play_by_play(Season = 2013)
# season2014 <- season_play_by_play(Season = 2014)
# season2015 <- season_play_by_play(Season = 2015)
# season2016 <- season_play_by_play(Season = 2016)
# season2017 <- season_play_by_play(Season = 2017)
# season2018 <- season_play_by_play(Season = 2018)
# 
# 
# 
# # binding into a single dataset
# all_seasons <- rbind(season2009, season2010, season2011, season2012, 
#                      season2013,season2014, season2015, season2016, 
#                      season2017, season2018)
# 
# write.csv(all_seasons, file = "all_seasons.csv", row.names = FALSE)
# saveRDS(all_seasons, "all_seasons.RData")
# 
# 
# 






tic()
# can start from here
all_seasons <- readRDS("~/SDP/epa/all_seasons.rds")










#### TRY CHANGING YEAR TO GET ENSURE THERE IS DRIVE_PONITS_NET == -2, OTHERWISE MODEL BREAKS
all_seasons <- all_seasons[ which(all_seasons$Season == 2010),]






# offensive TD flag. We assume all TDs are followed by XP or 2pt try, so if that team is attempting
# at [t+1], then the offense scored the touchdown on play [t].
all_seasons <- all_seasons[ which(is.na(all_seasons$ExPointResult)),]
all_seasons <- all_seasons[ which(is.na(all_seasons$TwoPointConv)),]
all_seasons$otd <- 0
for (k in 1:nrow(all_seasons)){
  all_seasons$otd[k] <- ifelse(all_seasons$Touchdown[k] == 1 & all_seasons$PlayType[k+1] == "Kickoff" &
                                 (all_seasons$posteam[k] != all_seasons$posteam[k+1]), 1, 0)
}
















# JAX issue
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "JAX", "JAC", all_seasons$DefensiveTeam)


# remove play under reviews
all_seasons <- all_seasons[ which(!is.na(all_seasons$down)),]
all_seasons$por <- ifelse(substr(all_seasons$desc, start=5, stop=21) == "play under review", 1, 0)
all_seasons <- all_seasons[ which( all_seasons$por == 0),]

# # remove challenges
# all_seasons$ChalReplayResult <- ifelse(is.na(all_seasons$ChalReplayResult), "NA", all_seasons$ChalReplayResult)
# all_seasons <- all_seasons[ which(all_seasons$ChalReplayResult != "Reversed"),]



# create defense-year and offense-year variables
all_seasons$offense_year <- paste(all_seasons$posteam, all_seasons$Season, sep = "_", collapse = NULL)
all_seasons$defense_year <- paste(all_seasons$DefensiveTeam, all_seasons$Season, sep = "_", collapse = NULL)




# create drive identifier
all_seasons$Drive2 <- all_seasons$Drive
all_seasons$Drive <- sprintf("%02d", as.numeric(all_seasons$Drive))
all_seasons$drive_unique <- paste(all_seasons$GameID, all_seasons$HomeTeam, all_seasons$Drive, sep = "_", 
                                  collapse = NULL)




# drive scoring result
all_seasons$drive_points <- ifelse(all_seasons$Safety == 1, -2, 
                                   ifelse(all_seasons$Touchdown == 1 & all_seasons$sp == 1, 7, 
                                          ifelse(all_seasons$FieldGoalResult == "Good", 3, 0)))



# remove OT
all_seasons <- all_seasons[ which(all_seasons$qtr != 5),]




# blocked punts and field goals
all_seasons$punt_block <- ifelse(all_seasons$PuntResult == "Blocked", 1, 0)
all_seasons$punt_block <- ifelse(is.na(all_seasons$punt_block), 0, all_seasons$punt_block)
all_seasons$fg_block <- ifelse(all_seasons$FieldGoalResult == "Blocked", 1, 0)
all_seasons$fg_block <- ifelse(is.na(all_seasons$fg_block), 0, all_seasons$fg_block)
all_seasons$block <- ifelse(all_seasons$punt_block == 1 | all_seasons$fg_block == 1, 1, 0)




# remove peanlty line items, NA downs
all_seasons <- all_seasons[ which(!is.na(all_seasons$down)),]




# identify defensive tds and special teams "defensive" tds, dtd
all_seasons$dtd <- 0
all_seasons$dtd <- ifelse(all_seasons$block == 1 & all_seasons$Touchdown == 1, 1, all_seasons$dtd)
all_seasons$ReturnResult <- ifelse(is.na(all_seasons$ReturnResult), 0, all_seasons$ReturnResult)
all_seasons$PuntResult <- ifelse(is.na(all_seasons$PuntResult), 0, all_seasons$PuntResult)
all_seasons$dtd <- ifelse(all_seasons$PuntResult == "Clean" & all_seasons$ReturnResult == "Touchdown", 1, all_seasons$dtd)
all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
for (k in 1:nrow(all_seasons) - 1){
  all_seasons$dtd[k] <- ifelse((all_seasons$Touchdown[k] == 1 & (all_seasons$posteam[k] == 
                                 all_seasons$posteam[k+1])), 1, 0)
  
}
all_seasons$ChalReplayResult <- ifelse( is.na(all_seasons$ChalReplayResult), 0, all_seasons$ChalReplayResult)
all_seasons$dtd <- ifelse(all_seasons$dtd == 1 & all_seasons$ChalReplayResult == "Reversed", 0, all_seasons$dtd)







# update drive scoring result for defensive TDs
all_seasons$drive_points <- ifelse(all_seasons$dtd == 1, -7, all_seasons$drive_points)




# get rid of pesky NAs
all_seasons$drive_points <- ifelse(is.na(all_seasons$drive_points),0, all_seasons$drive_points)




# one observation where TD, but ruled Safety after Challenge
all_seasons$drive_points <- ifelse(all_seasons$drive_points == 2, -2, all_seasons$drive_points)




# creating "next" drive identifier
all_seasons$drive_t_plus_1 <- all_seasons$Drive2 + 1
all_seasons$drive_t_plus_1 <- sprintf("%02d", as.numeric(all_seasons$drive_t_plus_1))
all_seasons$drive_unique_t_plus_1 <- paste(all_seasons$GameID, all_seasons$HomeTeam, 
                                           all_seasons$drive_t_plus_1, sep = "_", 
                                            collapse = NULL)








# create mapping betwen drive_unique and drive_points
all_seasons_reverse2 <- all_seasons
all_seasons_reverse2 <- all_seasons_reverse2[order(all_seasons_reverse2$drive_unique, 
                                                 all_seasons_reverse2$drive_points, decreasing=TRUE),]
all_seasons_unique2 <- all_seasons_reverse2[!duplicated(all_seasons_reverse2$drive_unique),]
myvars <- c("drive_unique", "drive_points")
all_seasons_unique2 <- all_seasons_unique2[myvars]
all_seasons_unique2[is.na(all_seasons_unique2)] <- 0




## rename drive_unique to be drive_unique_t_plus_1
names(all_seasons_unique2)[names(all_seasons_unique2) == 'drive_unique'] <- 'drive_unique_t_plus_1'




## rename drive_points to be drive_points_t_plus_1.
names(all_seasons_unique2)[names(all_seasons_unique2) == 'drive_points'] <- 'drive_points_t_plus_1'




## merge onto all_seasons by drive_unique_t_plus_1 (our identifier)
all_seasons <- merge(all_seasons,all_seasons_unique2,by="drive_unique_t_plus_1")




# keep desired PlayTypes
all_seasons$DesiredPlayType <- ifelse(all_seasons$PlayType == "Run" | all_seasons$PlayType == "Pass" 
                                      | all_seasons$PlayType == "Field Goal" | all_seasons$PlayType == "Sack" 
                                      | all_seasons$PlayType == "Punt",1,0)
all_seasons <- all_seasons[ which(all_seasons$DesiredPlayType == 1),]





# reverse sort
all_seasons_reverse <- all_seasons
all_seasons_reverse <- all_seasons_reverse[order(all_seasons_reverse$drive_unique, 
                                                 all_seasons_reverse$drive_points, decreasing=TRUE),]



# keep unique drives
all_seasons_unique <- all_seasons_reverse[!duplicated(all_seasons_reverse$drive_unique),]




# keep only drive identifier and drive points for dataset merge
myvars <- c("drive_unique", "drive_points")
all_seasons_unique <- all_seasons_unique[myvars]
all_seasons_unique[is.na(all_seasons_unique)] <- 0




# keep desired variables
myvars <- c("Date", "GameID", "Drive", "qtr", "down", "TimeSecs", "yrdline100", "ydstogo", "GoalToGo", 
            "Yards.Gained", "FirstDown", "posteam", "DefensiveTeam", "Passer", "offense_year", "defense_year", 
            "HomeTeam", "AwayTeam", "desc", "Touchdown", "Safety", "PlayType", "RushAttempt", "FieldGoalResult", 
            "PosTeamScore", "DefTeamScore", "ScoreDiff", "AbsScoreDiff", "dtd", "EPA", "drive_unique", 
            "DesiredPlayType", "ExpPts", "sp", "drive_points_t_plus_1", "RecFumbTeam", "PuntResult", "InterceptionThrown", 
            "ChalReplayResult", "PassOutcome", "otd")
all_seasons <- all_seasons[myvars]




# merge onto primary dataset
all_seasons <- merge(all_seasons,all_seasons_unique,by="drive_unique")




# reverse sort
all_seasons_reverse3 <- all_seasons
all_seasons_reverse3 <- all_seasons_reverse3[order(all_seasons_reverse3$drive_unique, 
                                                 -all_seasons_reverse3$dtd),]



# keep unique drives
all_seasons_unique3 <- all_seasons_reverse3[!duplicated(all_seasons_reverse3$drive_unique),]




# keep only drive identifier and dtd for dataset merge
myvars <- c("drive_unique", "dtd")
all_seasons_unique3 <- all_seasons_unique3[myvars]
all_seasons_unique3[is.na(all_seasons_unique3)] <- 0




# keep desired variables
myvars <- c("Date", "GameID", "Drive", "qtr", "down", "TimeSecs", "yrdline100", "ydstogo", "GoalToGo", 
            "Yards.Gained", "FirstDown", "posteam", "DefensiveTeam", "Passer", "offense_year", "defense_year", 
            "HomeTeam", "AwayTeam", "desc", "Touchdown", "Safety", "PlayType", "RushAttempt", "FieldGoalResult", 
            "PosTeamScore", "DefTeamScore", "ScoreDiff", "AbsScoreDiff", "EPA", "drive_unique", 
            "DesiredPlayType", "ExpPts", "sp", "drive_points", "drive_points_t_plus_1", "RecFumbTeam", "PuntResult", "InterceptionThrown",
            "ChalReplayResult", "PassOutcome", "otd")
all_seasons <- all_seasons[myvars]




# merge onto primary dataset
all_seasons <- merge(all_seasons,all_seasons_unique3,by="drive_unique")




# drop 2 pt conversions
all_seasons2 <- all_seasons[ !is.na(all_seasons$down),] 
all_seasons <- all_seasons2




# drive t score flag (for SpreadX, we always net with the outocme of the next scoring drive)
all_seasons$drive_t_score_flag <- ifelse(all_seasons$drive_points == 0, 0, 1)



# calculating drive_points_net
all_seasons$drive_points_net <- all_seasons$drive_points - all_seasons$drive_points_t_plus_1





# dtd == 1 for an entire drive which ends in a dtd, necessary for net points calc. Go back and make dtd play-specific
all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
for (k in 1:nrow(all_seasons) - 1){
  all_seasons$last_play_of_drive[k] <- ifelse(
    all_seasons$Drive[k] == all_seasons$Drive[k+1], 0, 1)
}
all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
for (k in 1:nrow(all_seasons) - 1){
  all_seasons$dtd[k] <- ifelse(all_seasons$dtd[k] == 1 & 
                                      all_seasons$last_play_of_drive[k] == 0, 0, all_seasons$dtd[k])
}



# dtd override
all_seasons$dtd <- ifelse(all_seasons$otd == 1, 0, all_seasons$dtd)




# sort 
all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 




# create turnover flag
all_seasons$turnover <- 0
for (k in 1:nrow(all_seasons) - 1){
  all_seasons$turnover[k] <- ifelse(all_seasons$drive_points_net[k] <= 0 & 
                                              all_seasons$posteam[k] != all_seasons$posteam[k+1], 1, 0)
}
all_seasons$turnover <- ifelse(all_seasons$dtd == 1, 1, all_seasons$turnover)






## remove end of half plays where team may not be trying to/be able to maximize expected points each play
# remove plays with < 4 min left in half plays
all_seasons$end_of_half <- ifelse(all_seasons$TimeSecs <= 2040 & all_seasons$TimeSecs >= 1800, 1, 0)

# keep full drives (Q2 issues)
all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
for (k in 1:nrow(all_seasons) - 1){
  all_seasons$end_of_half_remove[k] <- ifelse(all_seasons$end_of_half[k] == 1 & 
                                      all_seasons$Drive[k] == all_seasons$Drive[k+1], 1, 0)
}
all_seasons$end_of_half_remove <- ifelse(all_seasons$TimeSecs <= 1840 & all_seasons$TimeSecs >= 1800, 1, 0)
all_seasons <- all_seasons[ which(all_seasons$end_of_half_remove == 0 | is.na(all_seasons$end_of_half_remove)), ]


# remove Q4 when score differential is over 17
all_seasons$clock_melt_1 <- ifelse(all_seasons$qtr == 4 & all_seasons$AbsScoreDiff > 17, 1, 0)

# remove Q4 when score differential is over 8 and time less than 5 min (AbsScoreDiff > 8 & TimeSecs < 300)
all_seasons$clock_melt_2 <- ifelse(all_seasons$TimeSecs < 300 & all_seasons$AbsScoreDiff > 8, 1, 0)

# Q4 when less than 4 minutes, regardless of score
all_seasons$clock_melt_3 <- ifelse(all_seasons$TimeSecs < 240, 1, 0)


# combine Q4 clock melts
all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
for (k in 1:nrow(all_seasons)){
  all_seasons$clock_melt[k] <- max(all_seasons$clock_melt_1[k], all_seasons$clock_melt_2[k], all_seasons$clock_melt_3[k])
}


# keep full drives (Q4 issues)
all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
for (k in 1:nrow(all_seasons) - 1){
  all_seasons$clock_melt_remove[k] <- ifelse(all_seasons$clock_melt[k] == 1 & 
                                                all_seasons$Drive[k] == all_seasons$Drive[k+1], 1, 0)
}



# # clean up 111101111
# all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
# for (k in 2:nrow(all_seasons)){
#   all_seasons$clock_melt_remove[k] <- ifelse(all_seasons$clock_melt[k] == 1 & all_seasons$Drive[k] == all_seasons$Drive[k-1], 
#                                               1, all_seasons$clock_melt_remove[k])
# }
# all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
# for (k in 2:nrow(all_seasons)){
#   all_seasons$clock_melt_remove[k] <- ifelse(all_seasons$clock_melt_remove[k] == 1 & all_seasons$clock_melt_remove[k-1] == 0 
#                                              & all_seasons$Drive[k] == all_seasons$Drive[k-1], 0, all_seasons$clock_melt_remove[k])
# }
# all_seasons$clock_melt_remove[1] <- 0
# all_seasons$clock_melt_remove <- ifelse(all_seasons$TimeSecs <= 40, 1, 0)



# remove Q4 clock melts
all_seasons <- all_seasons[ which(all_seasons$clock_melt_remove != 1), ]









# sort 
all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
all_seasons$drive_points <- ifelse(all_seasons$dtd == 1, -7, all_seasons$drive_points)
all_seasons$drive_points_net <- ifelse(all_seasons$dtd == 1, -7, all_seasons$drive_points_net)



# remove temp datasets
rm(all_seasons_unique)
rm(all_seasons_reverse)
rm(all_seasons_unique2)
rm(all_seasons_reverse2)
rm(all_seasons_unique3)
rm(all_seasons_reverse3)
rm(all_seasons2)
# re(season2009) # keep these safe for now
# rm(season2010) 
# rm(season2011)
# rm(season2012)
# rm(season2013)
# rm(season2014)
# rm(season2015)
# rm(season2016)
# rm(season2017)
# rm(season2018)




# models







# MODEL 5: create multinomial logistic regression model 1 (w/ team effect, year-specific team)
Mod5 <- multinom(drive_points_net ~  factor(down) + ydstogo + yrdline100 + factor(offense_year) + 
                   factor(defense_year), 
                 data=all_seasons, MaxNWts = 5000)
print(Mod5)
# save model
saveRDS(Mod5, file = str_c(today(), "_multiMod3.rds"))









# predict for selected model
all_seasons_predict <- data.frame(all_seasons, y_hat = fitted(Mod5), e = residuals(Mod5))
all_seasons_predict$y_hat = all_seasons_predict$y_hat.3*3 + all_seasons_predict$y_hat.7*7 + 
                            all_seasons_predict$y_hat..7*(-7) + all_seasons_predict$y_hat..2*(-2)
                             + all_seasons_predict$y_hat..3*(-3) + all_seasons_predict$y_hat.2*(2) 
all_seasons_predict <- all_seasons_predict[order(all_seasons_predict$GameID, -all_seasons_predict$TimeSecs),] 


# sort 
all_seasons_predict <- all_seasons_predict[order(all_seasons_predict$GameID, -all_seasons_predict$TimeSecs),] 




# calculating True_EPA. Define y_hat_t_plus_1
all_seasons_predict$y_hat_t_plus_1 <- 0

for (k in 1:nrow(all_seasons_predict) - 1){
  all_seasons_predict$y_hat_t_plus_1[k] <- all_seasons_predict$y_hat[k+1]
  all_seasons_predict$y_hat_t_plus_1[nrow(all_seasons_predict)] <- 
    all_seasons_predict$drive_points_net[nrow(all_seasons_predict)]
}



# sort 
all_seasons_predict <- all_seasons_predict[order(all_seasons_predict$GameID, -all_seasons_predict$TimeSecs),] 











# calculating True_EPA. Change names of ExpPts vars
names(all_seasons_predict)[names(all_seasons_predict) == 'y_hat'] <- 'True_ExPts'
names(all_seasons_predict)[names(all_seasons_predict) == 'y_hat_t_plus_1'] <- 'True_ExPts_t_plus_1'




# calculating True_EPA. Create True_EPA var
all_seasons_predict$True_EPA <- ifelse(all_seasons_predict$turnover == 1, 
                                       (-1)*all_seasons_predict$True_ExPts_t_plus_1 - all_seasons_predict$True_ExPts,
                                       all_seasons_predict$True_ExPts_t_plus_1 - all_seasons_predict$True_ExPts)
all_seasons_predict$True_EPA <- ifelse(all_seasons_predict$dtd == 1 & all_seasons_predict$turnover == 1, 
                                       all_seasons_predict$drive_points_net - all_seasons_predict$True_ExPts, all_seasons_predict$True_EPA)
all_seasons_predict$True_EPA <- ifelse(all_seasons_predict$sp == 1, all_seasons_predict$drive_points - all_seasons_predict$True_ExPts,
                                       all_seasons_predict$True_EPA)






# sort 
all_seasons_predict <- all_seasons_predict[order(all_seasons_predict$GameID, -all_seasons_predict$TimeSecs),] 


myvars <- c("Date", "GameID", "Drive", "qtr", "down", "TimeSecs", "yrdline100", "ydstogo", "GoalToGo", 
            "Yards.Gained", "FirstDown", "posteam", "DefensiveTeam", "RecFumbTeam", "InterceptionThrown", "PassOutcome",
            "HomeTeam", "AwayTeam", "Passer", "offense_year", "defense_year", "desc", "Touchdown", "Safety", "PlayType", 
            "RushAttempt", "FieldGoalResult", "PosTeamScore", "PuntResult", "DefTeamScore", "ScoreDiff", "AbsScoreDiff", "drive_unique",
            "drive_points", "sp", "drive_points_t_plus_1", "drive_points_net", "ChalReplayResult", "dtd", "otd", "turnover",
            "ExpPts", "True_ExPts", "True_ExPts_t_plus_1", "EPA", "True_EPA")
all_seasons_predict <- all_seasons_predict[myvars]




all_seasons_predict$test <- abs(all_seasons_predict$EPA - all_seasons_predict$True_EPA)





# reconcile 1st and goal at the 1 bias?
# Second model where both teams get 1 drive (SpreadX)
# Third model which uses TrueEPA logic, (1 possession if score, 0 if no score) but calculate
  # scores before half? Don't give opponent extra drive
  # abs(TrueExp(t+1)) - abs(TrueExp(t)) and this determines how much a O/U should change after a given play (TotalX)
# Experiment with a fourth model which removes 4th downs early. Does this affect 1st and goal at the 1 bias?
# research what exactly EPA measures and how we might be different 
  # EPA looks at net score at end of half, and includes time left on clock as a variable, as well as home team. 
  # EPA is an efficiency metric. What is True_EPA?
  # True EPA is also an efficiency metric. If we want to see how a player impacts spread, then maybe allow for each team to get 1 drive.
# offensive fumbles followed by defensive fumbles recovered by offense need to not be counted as turnover == 1 
  # dtd == 0 & DefensiveTeam == RecFumbTeam & posteam[k] == posteam[k+1]
  # check values right before end of half
toc()
unique(all_seasons_predict$turnover)
