ncaa <- read.csv(file.path("C:/Users/Mike Sabsook/Desktop/Mike School/Northwestern/456 Sports Performance/assignment 4/training.csv"), header = FALSE, stringsAsFactors = FALSE) 
names(ncaa)<-c("school","year","player","draft_rnd","draft_pos","class","season","position","height","games","fgm","fga","fg_pct","x3m","x3a","x3_pct","ftm","fta","ft_pct","rbds","rpg","asts","apg","blks","bpg","stls","spg","pts","ppg","to")

ncaa$draft_rnd <- as.numeric(as.character(ncaa$draft_rnd))
ncaa$draft_pos <- as.numeric(as.character(ncaa$draft_pos))
ncaa$height <- as.numeric(as.character(ncaa$height))
ncaa$games <- as.numeric(as.character(ncaa$games))
ncaa$fgm <- as.numeric(as.character(ncaa$fgm))
ncaa$fga <- as.numeric(as.character(ncaa$fga))
ncaa$fg_pct <- as.numeric(as.character(ncaa$fg_pct))
ncaa$x3m <- as.numeric(as.character(ncaa$x3m))
ncaa$x3a <- as.numeric(as.character(ncaa$x3a))
ncaa$x3_pct <- as.numeric(as.character(ncaa$x3_pct))
ncaa$ftm <- as.numeric(as.character(ncaa$ftm))
ncaa$fta <- as.numeric(as.character(ncaa$fta))
ncaa$ft_pct <- as.numeric(as.character(ncaa$ft_pct))
ncaa$rbds <- as.numeric(as.character(ncaa$rbds))
ncaa$rpg <- as.numeric(as.character(ncaa$rpg))
ncaa$asts <- as.numeric(as.character(ncaa$asts))
ncaa$apg <- as.numeric(as.character(ncaa$apg))
ncaa$blks <- as.numeric(as.character(ncaa$blks))
ncaa$bpg <- as.numeric(as.character(ncaa$bpg))
ncaa$stls <- as.numeric(as.character(ncaa$stls))
ncaa$spg <- as.numeric(as.character(ncaa$spg))
ncaa$pts <- as.numeric(as.character(ncaa$pts))
ncaa$ppg <- as.numeric(as.character(ncaa$ppg))
ncaa$to <- as.numeric(as.character(ncaa$to))

ncaa["class"]<-substr(ncaa$class,start = 1, stop = 2)
ncaa$school <- factor(ncaa$school) 
ncaa$position <- factor(ncaa$position)
ncaa$year <- factor(ncaa$year)
ncaa$class <- factor(ncaa$class)
ncaa$player <- factor(ncaa$player)

summary(ncaa)

library(stats)
library(ggplot2)

#new turnovers per game variable
ncaa$tpg <- ncaa$to/ncaa$games


#new assists to turnovers ratio variable (need to be careful of multicollinearity in regression model)
ncaa$ast_to <- ncaa$asts/ncaa$to


#new column to indicate if player is drafted in the top 14 picks.  
#could change this to top5 or top 10 (or add new columns) if we want to.
ncaa$lottery <- ifelse(ncaa$draft_pos < 15, 1,0)

#######calculate career totals and averages of each player
#subset of only each player's final year of college with variables to be summed removed
draft_year_players <- subset(ncaa, draft_pos >0)


#Create a tiered list of schools

Tier1 <- c("Duke",
           "Kentucky",
           "Ohio St.",
           "Kansas",
           "Villanova")

Tier2 <- c("Wisconsin",
           "Syracuse",
           "Louisville",
           "Michigan St.",
           "North Carolina",
           "Arizona",
           "Florida",
           "Georgetown",
           "UCLA",
           "Baylor")

Tier3 <- c("Gonzaga",
           "Michigan",
           "Wichita St.",
           "Virginia",
           "Notre Dame",
           "Michigan",
           "New Mexico",
           "Missouri",
           "Indiana",
           "UConn",
           "Iowa St.")

Tier4 <- c("West Virginia",
           "San Diego St.",
           "Pittsburgh",
           "Kansas St.",
           "Creighton",
           "Vanderbilt",
           "Texas")

# Schools not in the four identified tiers will be identified with tier 5
draft_year_players$College_Tier <- rep(5, times = nrow(draft_year_players))
for (iplayer in seq(along = draft_year_players$player)) {
  if (draft_year_players$school[iplayer] %in% Tier1)
    draft_year_players$College_Tier[iplayer] <- 1
  if (draft_year_players$school[iplayer] %in% Tier2)
    draft_year_players$College_Tier[iplayer] <- 2
  if (draft_year_players$school[iplayer] %in% Tier3)
    draft_year_players$College_Tier[iplayer] <- 3  
  if (draft_year_players$school[iplayer] %in% Tier4)
    draft_year_players$College_Tier[iplayer] <- 4
}

# 3-tiered structure
# Schools not in the top two identified tiers will be identified with tier 3
draft_year_players$college_tier2 <- rep(3, times = nrow(draft_year_players))
for (iplayer in seq(along = draft_year_players$player)) {
  if (draft_year_players$school[iplayer] %in% Tier1)
    draft_year_players$college_tier2[iplayer] <- 1
  if (draft_year_players$school[iplayer] %in% Tier2)
    draft_year_players$college_tier2[iplayer] <- 2
}


#load packages for vif function to check multicollinearity and stepwise regression
library(car)
library(MASS)

#correlation matrices by position

source("http://www.sthda.com/upload/rquery_cormat.r")

#correlation to draft position

#subset by positions identified in data file
Guards <- subset(draft_year_players, position == 'G')
Forwards <- subset(draft_year_players, position == 'F')
Centers <- subset(draft_year_players, position == 'C')

#correlation to pick

rquery.cormat(Guards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30)])
rquery.cormat(Guards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30)], type = "flatten", graph = FALSE)

rquery.cormat(Forwards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30)])
rquery.cormat(Forwards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30)], type = "flatten", graph = FALSE)

rquery.cormat(Centers[,c(5,11,12,14,15,17,18,20,22,24,26,28,30)])
rquery.cormat(Centers[,c(5,11,12,14,15,17,18,20,22,24,26,28,30)], type = "flatten", graph = FALSE)

#correlation to lottery pick
rquery.cormat(Guards[,c(11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Guards[,c(11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)

rquery.cormat(Forwards[,c(11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Forwards[,c(11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)

rquery.cormat(Centers[,c(11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Centers[,c(11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)


#correlation to round picked

rquery.cormat(Guards[,c(4,11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Guards[,c(4,11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)

rquery.cormat(Forwards[,c(4,11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Forwards[,c(4,11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)

rquery.cormat(Centers[,c(4,11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Centers[,c(4,11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)

#####calculate transformed variables

draft_year_players$blkslog <- log(draft_year_players$blks+1)   
draft_year_players$astslog <- log(draft_year_players$asts+1)
draft_year_players$tolog <- log(draft_year_players$to+1)
draft_year_players$stlslog <-log(draft_year_players$stls+1)
draft_year_players$rbdslog <- log(draft_year_players$rbds+1)
draft_year_players$fgalog <- log(draft_year_players$fga+1)
draft_year_players$fgmlog <- log(draft_year_players$fgm+1)   
draft_year_players$ptslog <- log(draft_year_players$pts+1)       
draft_year_players$ftmlog <- log(draft_year_players$ftm+1)
draft_year_players$ftalog <- log(draft_year_players$fta+1)
draft_year_players$x3mlog <- log(draft_year_players$x3m+1)
draft_year_players$x3alog <- log(draft_year_players$x3a+1)



draft_year_players$blkssr <- sqrt(draft_year_players$blks)   
draft_year_players$astssr <- sqrt(draft_year_players$asts)
draft_year_players$tosr <- sqrt(draft_year_players$to)
draft_year_players$stlssr <-sqrt(draft_year_players$stls)
draft_year_players$rbdssr <- sqrt(draft_year_players$rbds)
draft_year_players$fgasr <- sqrt(draft_year_players$fga)
draft_year_players$fgmsr <- sqrt(draft_year_players$fgm)   
draft_year_players$ptssr <- sqrt(draft_year_players$pts)       
draft_year_players$ftmsr <- sqrt(draft_year_players$ftm)
draft_year_players$ftasr <- sqrt(draft_year_players$fta)
draft_year_players$x3msr <- sqrt(draft_year_players$x3m)
draft_year_players$x3asr <- sqrt(draft_year_players$x3a)

#subset by positions identified in data file with transformed variables added to data frames
Guards <- subset(draft_year_players, position == 'G')
Forwards <- subset(draft_year_players, position == 'F')
Centers <- subset(draft_year_players, position == 'C')


#Box Plots of Career Average Stats by Position
plot(draft_year_players$position, draft_year_players$blks, 
     main="Figure A1 \n Blocks by Position", 
     xlab="Position", ylab="Blocks", pch=19)


plot(draft_year_players$position, draft_year_players$blks, 
     main="Figure A2 \n Blocks by Position", 
     xlab="Position", ylab="Blocks", pch=19)

plot(draft_year_players$position, draft_year_players$to, 
     main="Figure A3 \n Turnovers by Position", 
     xlab="Position", ylab="Turnovers", pch=19)

plot(draft_year_players$position, draft_year_players$stls, 
     main="Figure A4 \n Steals by Position", 
     xlab="Position", ylab="Steals", pch=19)

plot(draft_year_players$position, draft_year_players$rbds, 
     main="Figure A5 \n Rebounds by Position", 
     xlab="Position", ylab="Rebounds", pch=19)

plot(draft_year_players$position, draft_year_players$fga, 
     main="Figure A6 \n Field Goals Attempted by Position", 
     xlab="Position", ylab="FGM", pch=19)

plot(draft_year_players$position, draft_year_players$fgm, 
     main="Figure A7 \n Field Goals Made by Position", 
     xlab="Position", ylab="FGM", pch=19)

plot(draft_year_players$position, draft_year_players$pts, 
     main="Figure A8 \n Points by Position", 
     xlab="Position", ylab="Points", pch=19)

plot(draft_year_players$position, draft_year_players$ftm, 
     main="Figure A9 \n Free Throws Made by Position", 
     xlab="Position", ylab="FTM", pch=19)

plot(draft_year_players$position, draft_year_players$fta, 
     main="Figure A10 \n Free Throws Attempted by Position", 
     xlab="Position", ylab="FTA", pch=19)

plot(draft_year_players$position, draft_year_players$x3m, 
     main="Figure A11 \n Three Pointers Made by Position", 
     xlab="Position", ylab="X3M", pch=19)

plot(draft_year_players$position, draft_year_players$x3a, 
     main="Figure A12 \n Three Pointers Attempted by Position", 
     xlab="Position", ylab="X3A", pch=19)

#create specific positions based on height.  Guards were split nearly evenly and so were forwards.
#calling point guards 6'4" or shorter and shooting guards are 6'5" or taller
#defining small forwards as 6'8" or shorter and power forwards as 6'9" or taller
#can use this positions to run correlations because just running guards does not correlate to assists which is odd.
Point_Guards <- subset(Guards, height < 77)
Shooting_Guards <- subset(Guards, height > 76)
Small_Forwards <- subset(Forwards, height < 81)
Power_Forwards <- subset(Forwards, height > 80)


###########histograms to determine the distribution of each variable
###########if right skewed (everything is bunched up on the LEFT side, use log transformation)
###########the best variables have a more normal distribution and will reduce p-value

hist(Point_Guards$blks) #use log
hist(Point_Guards$asts) #good
hist(Point_Guards$to) #good
hist(Point_Guards$stls) #use sq rt
hist(Point_Guards$rbds) #use sq rt
hist(Point_Guards$fga) #good
hist(Point_Guards$fgm) #good
hist(Point_Guards$pts) #good
hist(Point_Guards$ftm) #good
hist(Point_Guards$fta) #good
hist(Point_Guards$x3m) #use sq rt
hist(Point_Guards$x3a) #good
 "Log"
hist(Point_Guards$blkslog) 
hist(Point_Guards$astslog) 
hist(Point_Guards$tolog) 
hist(Point_Guards$stlslog) 
hist(Point_Guards$rbdslog) 
hist(Point_Guards$fgalog) 
hist(Point_Guards$fgmlog) 
hist(Point_Guards$ptslog)
hist(Point_Guards$ftmlog) 
hist(Point_Guards$ftalog) 
hist(Point_Guards$x3mlog)
hist(Point_Guards$x3alog)

"Square Root"

hist(Point_Guards$blkssr) 
hist(Point_Guards$astssr) 
hist(Point_Guards$tosr) 
hist(Point_Guards$stlssr) 
hist(Point_Guards$rbdssr) 
hist(Point_Guards$fgasr) 
hist(Point_Guards$fgmsr) 
hist(Point_Guards$ptssr)
hist(Point_Guards$ftmsr) 
hist(Point_Guards$ftasr) 
hist(Point_Guards$x3msr)
hist(Point_Guards$x3asr)


rquery.cormat(Point_Guards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Point_Guards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)



""
"Model A"

"Point Guard"
#linear regression poing guards models 
PGmodel <- lm(draft_pos~blkslog+asts+to+stlssr+rbdssr+fga+fgm+pts+ftm+fta+x3msr+x3a+College_Tier+class, data = Point_Guards)
PGsum <- summary(PGmodel)
PGsum
vif(PGmodel)

#Calculate RMSE
PGmse <-mean(residuals(PGmodel)^2)
PGM.RMSE <- sqrt(PGmse)
PGM.RMSE

"Final Stepwise auto-selection model"
PGstep <- stepAIC(PGmodel, direction = "both", trace = 0)
PGstep$anova
summary(PGstep)

#RMSE for stepwise function
PGstepmse <- mean(residuals(PGstep)^2)
PGstep.RMSE <- sqrt(PGstepmse)
PGstep.RMSE





########Shooting Guards
hist(Shooting_Guards$blks) #use log
hist(Shooting_Guards$asts) #use log
hist(Shooting_Guards$to) #good
hist(Shooting_Guards$stls) #use sq rt
hist(Shooting_Guards$rbds) #good
hist(Shooting_Guards$fga) #good
hist(Shooting_Guards$fgm) #good
hist(Shooting_Guards$pts) #good
hist(Shooting_Guards$ftm) #good
hist(Shooting_Guards$fta) #good
hist(Shooting_Guards$x3m) #good
hist(Shooting_Guards$x3a) #good
"Log"
hist(Shooting_Guards$blkslog) 
hist(Shooting_Guards$astslog) 
hist(Shooting_Guards$tolog) 
hist(Shooting_Guards$stlslog) 
hist(Shooting_Guards$rbdslog) 
hist(Shooting_Guards$fgalog) 
hist(Shooting_Guards$fgmlog) 
hist(Shooting_Guards$ptslog)
hist(Shooting_Guards$ftmlog) 
hist(Shooting_Guards$ftalog) 
hist(Shooting_Guards$x3mlog)
hist(Shooting_Guards$x3alog)

"Square Root"

hist(Shooting_Guards$blkssr) 
hist(Shooting_Guards$astssr) 
hist(Shooting_Guards$tosr) 
hist(Shooting_Guards$stlssr) 
hist(Shooting_Guards$rbdssr) 
hist(Shooting_Guards$fgasr) 
hist(Shooting_Guards$fgmsr) 
hist(Shooting_Guards$ptssr)
hist(Shooting_Guards$ftmsr) 
hist(Shooting_Guards$ftasr) 
hist(Shooting_Guards$x3msr)
hist(Shooting_Guards$x3asr)

rquery.cormat(Shooting_Guards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Shooting_Guards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)



#shooting guards linear regression models 
SGmodel <- lm(draft_pos~blkslog+astslog+to+stlssr+rbds+fga+fgm+pts++ftm+fta+x3a+x3mlog+College_Tier+class,data = Shooting_Guards)
SGsum <- summary(SGmodel)
SGsum
vif(SGmodel)

#calculate RMSE
SGmse <- mean(residuals(SGmodel)^2)
SG.RMSE <- sqrt(SGmse)
SG.RMSE

########Final shooting guard model
#Stepwise auto-selection model
SGstep <- stepAIC(SGmodel, direction = "both", trace=0)
SGstep$anova
summary (SGstep)

#RMSE for stepwise function
SGstepmse <- mean(residuals(SGstep)^2)
SGstep.RMSE <- sqrt(SGstepmse)
SGstep.RMSE


########Small Forwards

hist(Small_Forwards$blks) #use sq rt
hist(Small_Forwards$asts) #use log
hist(Small_Forwards$to) #good
hist(Small_Forwards$stls) #good
hist(Small_Forwards$rbds) #good
hist(Small_Forwards$fga) #use sq rt
hist(Small_Forwards$fgm) #use log
hist(Small_Forwards$pts) #use log
hist(Small_Forwards$ftm) #good
hist(Small_Forwards$fta) #use sq rt
hist(Small_Forwards$x3m) #good
hist(Small_Forwards$x3a) #good
"Log"
hist(Small_Forwards$blkslog) #use log
hist(Small_Forwards$astslog) #good
hist(Small_Forwards$tolog) #use log
hist(Small_Forwards$stlslog) #use log
hist(Small_Forwards$rbdslog) #use log
hist(Small_Forwards$fgalog) #good
hist(Small_Forwards$fgmlog) #use sq rt
hist(Small_Forwards$ptslog)
hist(Small_Forwards$ftmlog) #good
hist(Small_Forwards$ftalog) #use sq rt
hist(Small_Forwards$x3mlog)
hist(Small_Forwards$x3alog)

"Square Root"

hist(Small_Forwards$blkssr) #use log
hist(Small_Forwards$astssr) #good
hist(Small_Forwards$tosr) #use log
hist(Small_Forwards$stlssr) #use log
hist(Small_Forwards$rbdssr) #use log
hist(Small_Forwards$fgasr) #good
hist(Small_Forwards$fgmsr) #use sq rt
hist(Small_Forwards$ptssr)
hist(Small_Forwards$ftmsr) #good
hist(Small_Forwards$ftasr) #use sq rt
hist(Small_Forwards$x3msr)
hist(Small_Forwards$x3asr)

rquery.cormat(Small_Forwards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Small_Forwards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)


#small forwards linear regression models 
SFmodel <- lm(draft_pos~blkssr+astslog+to+stls+rbds+fgasr+fgmlog+ptslog+ftm+ftasr+x3m+x3a+College_Tier+class,data = Small_Forwards)
SFsum <- summary(SFmodel)
SFsum
vif(SFmodel)

#calculate RMSE
SFmse <- mean(residuals(SFmodel)^2)
SF.RMSE <- sqrt(SFmse)
SF.RMSE

#Stepwise auto-selection model small forward
SFstep <- stepAIC(SFmodel, direction = "both", trace=0)
SFstep$anova
summary (SFstep)

#RMSE for stepwise function
SFstepmse <- mean(residuals(SFstep)^2)
SFstep.RMSE <- sqrt(SFstepmse)
SFstep.RMSE


"Power Forward"

hist(Power_Forwards$blks) #use log
hist(Power_Forwards$asts) #use sq rt
hist(Power_Forwards$to) #good
hist(Power_Forwards$stls) #use log
hist(Power_Forwards$rbds) #good
hist(Power_Forwards$fga) #good
hist(Power_Forwards$fgm) #good
hist(Power_Forwards$pts) #use log
hist(Power_Forwards$ftm) #good
hist(Power_Forwards$fta) #good
hist(Power_Forwards$x3m) #use log
hist(Power_Forwards$x3a) #use log
"Log"
hist(Power_Forwards$blkslog) 
hist(Power_Forwards$astslog) 
hist(Power_Forwards$tolog) 
hist(Power_Forwards$stlslog) 
hist(Power_Forwards$rbdslog) 
hist(Power_Forwards$fgalog) 
hist(Power_Forwards$fgmlog) 
hist(Power_Forwards$ptslog)
hist(Power_Forwards$ftmlog) 
hist(Power_Forwards$ftalog) 
hist(Power_Forwards$x3mlog)
hist(Power_Forwards$x3alog)

"Square Root"

hist(Power_Forwards$blkssr) 
hist(Power_Forwards$astssr) 
hist(Power_Forwards$tosr) 
hist(Power_Forwards$stlssr) 
hist(Power_Forwards$rbdssr) 
hist(Power_Forwards$fgasr) 
hist(Power_Forwards$fgmsr) 
hist(Power_Forwards$ptssr)
hist(Power_Forwards$ftmsr) 
hist(Power_Forwards$ftasr) 
hist(Power_Forwards$x3msr)
hist(Power_Forwards$x3asr)

rquery.cormat(Power_Forwards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Power_Forwards[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)



#power forwards linear regression models  
PFmodel <- lm(draft_pos~blkslog+astssr+to+stlslog+rbds+fga+fgm+ptslog+ftm+fta+x3mlog+x3alog+College_Tier+class,data = Power_Forwards)
PFsum <- summary(PFmodel)
PFsum
vif(PFmodel)


#calculate RMSE
PFmse <- mean(residuals(PFmodel)^2)
PF.RMSE <- sqrt(PFmse)
PF.RMSE

########Final power forward model
#Stepwise auto-selection model
PFstep <- stepAIC(PFmodel, direction = "both", trace=0)
PFstep$anova
summary (PFstep)

#RMSE for stepwise function
PFstepmse <- mean(residuals(PFstep)^2)
PFstep.RMSE <- sqrt(PFstepmse)
PFstep.RMSE



########Centers
hist(Centers$blks) #use log
hist(Centers$asts) #good
hist(Centers$to) #good
hist(Centers$stls) #use log
hist(Centers$rbds) #use sq rt
hist(Centers$fga) #good
hist(Centers$fgm) #use sq rt
hist(Centers$pts) #good
hist(Centers$ftm) #use sq rt
hist(Centers$fta) #good
hist(Centers$x3m) #use log
hist(Centers$x3a) #use log
"Log"
hist(Centers$blkslog) 
hist(Centers$astslog) 
hist(Centers$tolog) 
hist(Centers$stlslog) 
hist(Centers$rbdslog) 
hist(Centers$fgalog) 
hist(Centers$fgmlog) 
hist(Centers$ptslog)
hist(Centers$ftmlog) 
hist(Centers$ftalog) 
hist(Centers$x3mlog)
hist(Centers$x3alog)

"Square Root"

hist(Centers$blkssr) 
hist(Centers$astssr) 
hist(Centers$tosr) 
hist(Centers$stlssr) 
hist(Centers$rbdssr) 
hist(Centers$fgasr) 
hist(Centers$fgmsr) 
hist(Centers$ptssr)
hist(Centers$ftmsr) 
hist(Centers$ftasr) 
hist(Centers$x3msr)
hist(Centers$x3asr)



rquery.cormat(Centers[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)])
rquery.cormat(Centers[,c(5,11,12,14,15,17,18,20,22,24,26,28,30,33)], type = "flatten", graph = FALSE)



#Centers linear regression models 

Cenmodel <- lm(draft_pos~blkslog+asts+to+stlslog+rbdssr+fga+fgmsr+pts+ftmsr+fta+x3mlog+x3alog+College_Tier+class,data = Centers)
Censum <- summary(Cenmodel)
Censum
vif(Cenmodel)

#calculate RMSE
Cenmse <- mean(residuals(Cenmodel)^2)
Cen.RMSE <- sqrt(Cenmse)
Cen.RMSE

########Final Centers model
#Stepwise auto-selection model
Censtep <- stepAIC(Cenmodel, direction = "both", trace=0)
Censtep$anova
summary (Censtep)

#RMSE for stepwise function
Censtepmse <- mean(residuals(Censtep)^2)
Censtep.RMSE <- sqrt(Censtepmse)
Censtep.RMSE






########### Point Guard Cross-validation
PGeval_model <- function(PGtraining_input, PGtest_input) {
  # for demonstration purposes we use a simple linear regression of Pick on Ht
  my_PGmodel <- lm(draft_pos~blkslog+asts+to+stlssr+rbdssr+fga+fgm+pts+ftm+fta+x3msr+x3a+College_Tier+class, data = Point_Guards)
  # predict the Pick response variable in test set 
  PGresponse_predict <- predict.lm(my_PGmodel, newdata = PGtest_input) 
  PGresponse_actual <- PGtest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((PGresponse_predict - PGresponse_actual)^2))
}   


# Whatever model is used for prediction, we want it to do better than a null model
# that predicts the mean response value for every player. Null model is like no model.
PGnull_model <- function(PGtraining_input, PGtest_input) {
  # for demonstration purposes we show what would be the prediction 
  # of a null model... predicting the mean Pick for every player in test_input
  PGresponse_predict <- mean(PGtraining_input$draft_pos)
  PGresponse_actual <- PGtest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((PGresponse_predict - PGresponse_actual)^2))
}       

library(cvTools)
set.seed(9999)  # for reproducibility   
PGnfolds <- 10                  

PGstudy_folds <- cvFolds(nrow(Point_Guards), K = PGnfolds, type = 'consecutive')

PGcv_model_results <- numeric(PGnfolds)  # initialize array to store fold model results
PGcv_null_results <- numeric(PGnfolds)  # initialize array to store fold null results
for (PGifold in 1:PGnfolds) {
  PGthis_fold_test_data <- Point_Guards[PGstudy_folds$which == PGifold,]
  PGthis_fold_training_data <- 
    Point_Guards[PGstudy_folds$which != PGifold,]
  # fit model and get root mean-square error for this iteration   
  PGcv_model_results[PGifold] <- PGeval_model(PGtraining_input = PGthis_fold_training_data,
                                              PGtest_input = PGthis_fold_test_data)    
  PGcv_null_results[PGifold] <- PGnull_model(PGtraining_input = PGthis_fold_training_data,
                                             PGtest_input = PGthis_fold_test_data)    
}

cat('\n', 'Cross-validation Null Point Guard Model Average Root Mean-Square Error:', 
    mean(PGcv_null_results))   

cat('\n', 'Cross-validation My Point Guard Model Average Root Mean-Square Error:', 
    mean(PGcv_model_results)) 

########### Shooting Guard Cross-validation work

set.seed(9999)  # for reproducibility   
SGnfolds <- 10

SGeval_model <- function(SGtraining_input, SGtest_input) {
  # for demonstration purposes we use a simple linear regression of Pick on Ht
  my_SGmodel <- lm(draft_pos~blkslog+astslog+to+stlssr+rbds+fga+fgm+pts++ftm+fta+x3m+x3a+College_Tier+class, data = Shooting_Guards)
  # predict the Pick response variable in test set 
  SGresponse_predict <- predict.lm(my_SGmodel, newdata = SGtest_input)
  SGresponse_actual <- SGtest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((SGresponse_predict - SGresponse_actual)^2))
} 


# Whatever model is used for prediction, we want it to do better than a null model
# that predicts the mean response value for every player. Null model is like no model.
SGnull_model <- function(SGtraining_input, SGtest_input) {
  # for demonstration purposes we show what would be the prediction 
  # of a null model... predicting the mean Pick for every player in test_input
  SGresponse_predict <- mean(SGtraining_input$draft_pos)
  SGresponse_actual <- SGtest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((SGresponse_predict - SGresponse_actual)^2))
}       

SGstudy_folds <- cvFolds(nrow(Shooting_Guards), K = SGnfolds, type = 'consecutive')

SGcv_model_results <- numeric(SGnfolds)  # initialize array to store fold model results
SGcv_null_results <- numeric(SGnfolds)  # initialize array to store fold null results
for (SGifold in 1:SGnfolds) {
  SGthis_fold_test_data <- Shooting_Guards[SGstudy_folds$which == SGifold,]
  SGthis_fold_training_data <- 
    Shooting_Guards[SGstudy_folds$which != SGifold,]
  # fit model and get root mean-square error for this iteration   
  SGcv_model_results[SGifold] <- SGeval_model(SGtraining_input = SGthis_fold_training_data,
                                              SGtest_input = SGthis_fold_test_data)    
  SGcv_null_results[SGifold] <- SGnull_model(SGtraining_input = SGthis_fold_training_data,
                                             SGtest_input = SGthis_fold_test_data)    
}


cat('\n', 'Cross-validation Null Shooting Guard Model Average Root Mean-Square Error:', 
    mean(SGcv_null_results))   

cat('\n', 'Cross-validation My Shooting Guard Model Average Root Mean-Square Error:', 
    mean(SGcv_model_results))

########### Small Forward Cross-validation work

set.seed(9999)  # for reproducibility   
SFnfolds <- 10


SFeval_model <- function(SFtraining_input, SFtest_input) {
  # for demonstration purposes we use a simple linear regression of Pick on Ht
  my_SFmodel <- lm(draft_pos~blkssr+astslog+to+stls+rbds+fgasr+fgmlog+ptslog+ftm+ftasr+x3m+x3a+College_Tier+class,data = Small_Forwards)
  "Stepwise auto-selection model"
  # predict the Pick response variable in test set 
  SFresponse_predict <- predict.lm(my_SFmodel, newdata = SFtest_input) 
  SFresponse_actual <- SFtest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((SFresponse_predict - SFresponse_actual)^2))
}   

# Whatever model is used for prediction, we want it to do better than a null model
# that predicts the mean response value for every player. Null model is like no model.
SFnull_model <- function(SFtraining_input, SFtest_input) {
  # for demonstration purposes we show what would be the prediction 
  # of a null model... predicting the mean Pick for every player in test_input
  SFresponse_predict <- mean(SFtraining_input$draft_pos)
  SFresponse_actual <- SFtest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((SFresponse_predict - SFresponse_actual)^2))
}       


SFstudy_folds <- cvFolds(nrow(Small_Forwards), K = SFnfolds, type = 'consecutive')


SFcv_model_results <- numeric(SFnfolds)  # initialize array to store fold model results
SFcv_null_results <- numeric(SFnfolds)  # initialize array to store fold null results
for (SFifold in 1:SFnfolds) {
  SFthis_fold_test_data <- Small_Forwards[SFstudy_folds$which == SFifold,]
  SFthis_fold_training_data <- 
    Small_Forwards[SFstudy_folds$which != SFifold,]
  # fit model and get root mean-square error for this iteration   
  SFcv_model_results[SFifold] <- SFeval_model(SFtraining_input = SFthis_fold_training_data,
                                              SFtest_input = SFthis_fold_test_data)    
  SFcv_null_results[SFifold] <- SFnull_model(SFtraining_input = SFthis_fold_training_data,
                                             SFtest_input = SFthis_fold_test_data)    
}

cat('\n', 'Cross-validation Null Small Forward Model Average Root Mean-Square Error:', 
    mean(SFcv_null_results))   

cat('\n', 'Cross-validation My Small Forward Model Average Root Mean-Square Error:', 
    mean(SFcv_model_results))

########### Power Forward Cross-validation work
set.seed(9999)  # for reproducibility   
PFnfolds <- 10

PFeval_model <- function(PFtraining_input, PFtest_input) {
  # for demonstration purposes we use a simple linear regression of Pick on Ht
  my_PFmodel <- lm(draft_pos~blkslog+astssr+to+stlslog+rbds+fga+fgm+ptslog+ftm+fta+x3mlog+x3alog+College_Tier+class,data = Power_Forwards)
  # predict the Pick response variable in test set 
  PFresponse_predict <- predict.lm(my_PFmodel, newdata= PFtest_input) 
  PFresponse_actual <- PFtest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((PFresponse_predict - PFresponse_actual)^2))
}   

# Whatever model is used for prediction, we want it to do better than a null model
# that predicts the mean response value for every player. Null model is like no model.
PFnull_model <- function(PFtraining_input, PFtest_input) {
  # for demonstration purposes we show what would be the prediction 
  # of a null model... predicting the mean Pick for every player in test_input
  PFresponse_predict <- mean(PFtraining_input$draft_pos)
  PFresponse_actual <- PFtest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((PFresponse_predict - PFresponse_actual)^2))
}  

PFstudy_folds <- cvFolds(nrow(Power_Forwards), K = PFnfolds, type = 'consecutive')

PFcv_model_results <- numeric(PFnfolds)  # initialize array to store fold model results
PFcv_null_results <- numeric(PFnfolds)  # initialize array to store fold null results
for (PFifold in 1:PFnfolds) {
  PFthis_fold_test_data <- Power_Forwards[PFstudy_folds$which == PFifold,]
  PFthis_fold_training_data <- 
    Power_Forwards[PFstudy_folds$which != PFifold,]
  # fit model and get root mean-square error for this iteration   
  PFcv_model_results[PFifold] <- PFeval_model(PFtraining_input = PFthis_fold_training_data,
                                              PFtest_input = PFthis_fold_test_data)    
  PFcv_null_results[PFifold] <- PFnull_model(PFtraining_input = PFthis_fold_training_data,
                                             PFtest_input = PFthis_fold_test_data)    
}

cat('\n', 'Cross-validation Null Power Forward Model Average Root Mean-Square Error:', 
    mean(PFcv_null_results))   

cat('\n', 'Cross-validation My Power Forward Model Average Root Mean-Square Error:', 
    mean(PFcv_model_results))

########### Center Cross-validation work
set.seed(9999)  # for reproducibility   
Cennfolds <- 10

Ceneval_model <- function(Centraining_input, Centest_input) {
  # for demonstration purposes we use a simple linear regression of Pick on Ht
  my_Cenmodel <- lm(draft_pos~blkslog+asts+to+stlslog+rbdssr+fga+fgmsr+pts+ftmsr+fta+x3mlog+x3alog+College_Tier+class,data = Centers)
  # predict the Pick response variable in test set 
  Cenresponse_predict <- predict.lm(my_Cenmodel, newdata = Centest_input) 
  Cenresponse_actual <- Centest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((Cenresponse_predict - Cenresponse_actual)^2))
}  

# Whatever model is used for prediction, we want it to do better than a null model
# that predicts the mean response value for every player. Null model is like no model.
Cennull_model <- function(Centraining_input, Centest_input) {
  # for demonstration purposes we show what would be the prediction 
  # of a null model... predicting the mean Pick for every player in test_input
  Cenresponse_predict <- mean(Centraining_input$draft_pos)
  Cenresponse_actual <- Centest_input$draft_pos
  # compute and return root mean-square error in test_input
  sqrt(mean((Cenresponse_predict - Cenresponse_actual)^2))
}       

Censtudy_folds <- cvFolds(nrow(Centers), K = Cennfolds, type = 'consecutive')

Cencv_model_results <- numeric(Cennfolds)  # initialize array to store fold model results
Cencv_null_results <- numeric(Cennfolds)  # initialize array to store fold null results
for (Cenifold in 1:Cennfolds) {
  Centhis_fold_test_data <- Centers[Censtudy_folds$which == Cenifold,]
  Centhis_fold_training_data <- 
    Centers[Censtudy_folds$which != Cenifold,]
  # fit model and get root mean-square error for this iteration   
  Cencv_model_results[Cenifold] <- Ceneval_model(Centraining_input = Centhis_fold_training_data,
                                                 Centest_input = Centhis_fold_test_data)    
  Cencv_null_results[Cenifold] <- Cennull_model(Centraining_input = Centhis_fold_training_data,
                                                Centest_input = Centhis_fold_test_data)    
}

cat('\n', 'Cross-validation Null Center Model Average Root Mean-Square Error:', 
    mean(Cencv_null_results))   

cat('\n', 'Cross-validation My Center Model Average Root Mean-Square Error:', 
    mean(Cencv_model_results))
#################test set prep######################
ncaa_test <- read.csv(file.path("C:/Users/Mike Sabsook/Desktop/Mike School/Northwestern/456 Sports Performance/assignment 4/test.csv"), header = TRUE, stringsAsFactors = FALSE) 
names(ncaa_test)<-c("school","year","player","draft_rnd","draft_pos","class","season","position","height","games","fgm","fga","fg_pct","x3m","x3a","x3_pct","ftm","fta","ft_pct","rbds","rpg","asts","apg","blks","bpg","stls","spg","pts","ppg","to")


ncaa_test$draft_rnd <- as.numeric(as.character(ncaa_test$draft_rnd))
ncaa_test$draft_pos <- as.numeric(as.character(ncaa_test$draft_pos))
ncaa_test$height <- as.numeric(as.character(ncaa_test$height))
ncaa_test$games <- as.numeric(as.character(ncaa_test$games))
ncaa_test$fgm <- as.numeric(as.character(ncaa_test$fgm))
ncaa_test$fga <- as.numeric(as.character(ncaa_test$fga))
ncaa_test$fg_pct <- as.numeric(as.character(ncaa_test$fg_pct))
ncaa_test$x3m <- as.numeric(as.character(ncaa_test$x3m))
ncaa_test$x3a <- as.numeric(as.character(ncaa_test$x3a))
ncaa_test$x3_pct <- as.numeric(as.character(ncaa_test$x3_pct))
ncaa_test$ftm <- as.numeric(as.character(ncaa_test$ftm))
ncaa_test$fta <- as.numeric(as.character(ncaa_test$fta))
ncaa_test$ft_pct <- as.numeric(as.character(ncaa_test$ft_pct))
ncaa_test$rbds <- as.numeric(as.character(ncaa_test$rbds))
ncaa_test$rpg <- as.numeric(as.character(ncaa_test$rpg))
ncaa_test$asts <- as.numeric(as.character(ncaa_test$asts))
ncaa_test$apg <- as.numeric(as.character(ncaa_test$apg))
ncaa_test$blks <- as.numeric(as.character(ncaa_test$blks))
ncaa_test$bpg <- as.numeric(as.character(ncaa_test$bpg))
ncaa_test$stls <- as.numeric(as.character(ncaa_test$stls))
ncaa_test$spg <- as.numeric(as.character(ncaa_test$spg))
ncaa_test$pts <- as.numeric(as.character(ncaa_test$pts))
ncaa_test$ppg <- as.numeric(as.character(ncaa_test$ppg))
ncaa_test$to <- as.numeric(as.character(ncaa_test$to))

ncaa_test["class"]<-substr(ncaa_test$class,start = 1, stop = 2)
ncaa_test$class <- factor(ncaa_test$class)
ncaa_test$school <- factor(ncaa_test$school) 
ncaa_test$position <- factor(ncaa_test$position)
ncaa_test$year <- factor(ncaa_test$year)
ncaa_test$class <- factor(ncaa_test$class)
ncaa_test$player <- factor(ncaa_test$player)

#new turnovers per game variable
ncaa_test$tpg <- ncaa_test$to/ncaa_test$games

#new assists to turnovers ratio variable (need to be careful of multicollinearity in regression model)
ncaa_test$ast_to <- ncaa_test$asts/ncaa_test$to

#new column to indicate if player is drafted in the top 14 picks.  
#could change this to top5 or top 10 (or add new columns) if we want to.
ncaa_test$lottery <- ifelse(ncaa_test$draft_pos < 15, 1,0)


# Schools not in the four identified tiers will be identified with tier 5
ncaa_test$College_Tier <- rep(5, times = nrow(ncaa_test))
for (iplayer in seq(along = ncaa_test$player)) {
  if (ncaa_test$school[iplayer] %in% Tier1)
    ncaa_test$College_Tier[iplayer] <- 1
  if (ncaa_test$school[iplayer] %in% Tier2)
    ncaa_test$College_Tier[iplayer] <- 2
  if (ncaa_test$school[iplayer] %in% Tier3)
    ncaa_test$College_Tier[iplayer] <- 3  
  if (ncaa_test$school[iplayer] %in% Tier4)
    ncaa_test$College_Tier[iplayer] <- 4
}

# alternate 3-tiered structure for examination
# Schools not in the top two identified tiers will be identified with tier 3
ncaa_test$college_tier2 <- rep(3, times = nrow(ncaa_test))
for (iplayer in seq(along = ncaa_test$player)) {
  if (ncaa_test$school[iplayer] %in% Tier1)
    ncaa_test$college_tier2[iplayer] <- 1
  if (ncaa_test$school[iplayer] %in% Tier2)
    ncaa_test$college_tier2[iplayer] <- 2
}

#####calculate transformed variables for possible use
ncaa_test$blkslog <- log(ncaa_test$blks+1)
ncaa_test$astslog <- log(ncaa_test$asts+1)
ncaa_test$tolog <- log(ncaa_test$to+1)
ncaa_test$stlslog <- log(ncaa_test$stls+1)
ncaa_test$rbdslog <- log(ncaa_test$rbds+1)
ncaa_test$fgalog <- log(ncaa_test$fga+1)
ncaa_test$fgmlog <- log(ncaa_test$fgm+1)
ncaa_test$ptslog <- log(ncaa_test$pts+1)
ncaa_test$ftmlog <- log(ncaa_test$ftm+1)
ncaa_test$ftalog <- log(ncaa_test$fta+1)
ncaa_test$x3mlog <- log(ncaa_test$x3m+1)
ncaa_test$x3alog <- log(ncaa_test$x3a+1)


ncaa_test$blkssr <- sqrt(ncaa_test$blks)
ncaa_test$astssr <- sqrt(ncaa_test$asts)
ncaa_test$tosr <- sqrt(ncaa_test$to)
ncaa_test$stlssr <- sqrt(ncaa_test$stls)
ncaa_test$rbdssr <- sqrt(ncaa_test$rbds)
ncaa_test$fgasr <- sqrt(ncaa_test$fga)
ncaa_test$fgmsr <- sqrt(ncaa_test$fgm)
ncaa_test$ptssr <- sqrt(ncaa_test$pts)
ncaa_test$ftmsr <- sqrt(ncaa_test$ftm)
ncaa_test$ftasr <- sqrt(ncaa_test$fta)
ncaa_test$x3msr <- sqrt(ncaa_test$x3m)
ncaa_test$x3asr <- sqrt(ncaa_test$x3a)

#subset by positions identified in data file
Guards_test <- subset(ncaa_test, position == 'G')
Forwards_test <- subset(ncaa_test, position == 'F')
Centers_test <- subset(ncaa_test, position == 'C')

#create specific positions based on height.  Guards were split nearly evenly and so were forwards.
#calling point guards 6'4" or shorter and shooting guards are 6'5" or taller
#defining small forwards as 6'8" or shorter and power forwards as 6'9" or taller
#can use this positions to run correlations because just running guards does not correlate to assists which is odd.
Point_Guards_test <- subset(Guards_test, height < 77)
Shooting_Guards_test <- subset(Guards_test, height > 76)
Small_Forwards_test <- subset(Forwards_test, height < 81)
Power_Forwards_test <- subset(Forwards_test, height > 80)
Point_Guards_test$prediction <- predict(PGstep, newdata = Point_Guards_test, type="response")
Shooting_Guards_test$prediction <- predict(SGstep, newdata = Shooting_Guards_test, type="response")
Small_Forwards_test$prediction <- predict(SFstep, newdata = Small_Forwards_test, type="response")
Power_Forwards_test$prediction <- predict(PFstep, newdata = Power_Forwards_test, type="response")
Centers_test$prediction <- predict(Censtep, newdata = Centers_test, type="response")
modelA_predictions <- rbind(Point_Guards_test, Shooting_Guards_test, Small_Forwards_test, Power_Forwards_test, Centers_test, by="prediction")
modelA_predictions <- modelA_predictions[c(3,6,1,8,4,5,60)]
modelA_predictions <-modelA_predictions[1:46,]
write.csv(modelA_predictions, file = "C:/Users/Mike Sabsook/Desktop/Mike School/Northwestern/456 Sports Performance/assignment 4/ModelA Predictions.csv", row.names = FALSE)
