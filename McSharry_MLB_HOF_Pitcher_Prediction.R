## Patricia McSharry, HarvardX: PH125.9x Data Science Certificate
## MLB Hall of Fame Pitcher Prediction - CYO Captsone Project

# install packages as needed
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Lahman)) install.packages("Lahman", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# year of latest data in lahman datset
latestyear = max(Pitching$yearID)

# use Pitching table, which stores data on a yearly basis
# group by playerID and calcuation totals wins, total loses, total saves, total hits, total bases on balls
# total earned runs, total outs pitched, total strike outs, and years played
# per HOF eligibility rules, players need 10 years in the majors to qualify for the HOF, filter out
# players with less than 10 years in the majors
PitchingSum <- Pitching %>%
  group_by(playerID) %>%
  summarize(tW = sum(W), tL = sum(L), tSV = sum(SV),
    tH = sum(H), tBB = sum(BB),  tER = sum(ER), 
	tIPouts=sum(IPouts), tSO = sum(SO),
	yearsPlayed = n()) %>% 
	filter(yearsPlayed  > 9)

# players need to be retired 5 years to be HOF eligible, store in table HOFEligible table
HOFEligible <- Master %>% filter(substring(finalGame,1,4) < toString(latestyear - 5)) %>%
select(playerID)

# code for special rule about a player dying in the past year, store in adjustEligible table
adjustEligible <- semi_join(Pitching, (Master %>% filter(as.numeric(format(Master$deathDate, "%Y")) == latestyear & as.numeric(substr(Master$finalGame,1,4)) == latestyear) ), by="playerID")

# adjust the HOFEligible table to account for players dying in the past year
HOFEligibile <- semi_join(HOFEligible , adjustEligible, by="playerID")

# join the PitchingSum table to the HOFEligible table to account for all HOF eligibiliy rules
PitchingSum <- semi_join(PitchingSum,HOFEligible)

# calculate career ERA (earned run average)
PitchingSum <- PitchingSum %>%
  mutate(tERA = (tER/(tIPouts / 3)) *9)

# calcualte career WHIP (walks plus hits per inning pitched for career)
PitchingSum <- PitchingSum %>%
  mutate(tWHIP = (tH + tBB) / (tIPouts / 3))

# calculate career winning percentage
PitchingSum <- PitchingSum %>%
  mutate(tWP = tW/ (tW + tL))  

# the hall of fame has managers and players inducted, create a table of all players in the HOF
IndHOF <- HallOfFame %>% filter(inducted == 'Y' & category=='Player') %>% select(playerID) %>% mutate(induct = 'Y')

# creat table PSumHOF by joining PitchingSum and IndHOF, which gives a pitching career level
# table.  induct column is Y if player is in the HOF and N if not
# induct is the column to predict
PSumHOF <-
  left_join(PitchingSum, IndHOF, by='playerID') 

# set induct to 'N' if player is not in the HOF
PSumHOF <- PSumHOF %>%
  mutate(induct = coalesce(induct, 'N'))  

# induct column needs to be a factor
PSumHOF <- PSumHOF %>%
  mutate(induct = as.factor(PSumHOF$induct))  

# create train and test sets
set.seed(1) 
test_index <- createDataPartition(y = PSumHOF$induct, times = 1, p = 0.5, list = FALSE)
trainPitch <- PSumHOF[-test_index,]
testPitch <- PSumHOF[test_index,]

# use random forest 

# randomForest with all predictors 
set.seed(1)
fit <- randomForest(induct ~ 
tW + tL + tSV + tH + tBB + tER + tIPouts + tSO + yearsPlayed + tERA + tWHIP + tWP,
data=trainPitch)
pitchhof_results <- data_frame(model = "All Predictors", 
  Accuracy = confusionMatrix(predict(fit, testPitch), testPitch$induct)$overall["Accuracy"])
# print out the table
pitchhof_results %>% knitr::kable()

# list importance of predictors, note that total wins and total innings pitched are the most important
importance(fit)

# use randomForest with total wins predictor   
set.seed(1)
fit2 <- randomForest(induct ~ tW ,  data=trainPitch)

# Using results of training, predict induct in the test set, save accuracy in pitchof_results table
pitchhof_results <- bind_rows(pitchhof_results,
  data_frame(model="Total Wins",  
  Accuracy=confusionMatrix(predict(fit2, testPitch), testPitch$induct)$overall["Accuracy"]))

# use randomForest with total innings pitched predictor   
set.seed(1)  
fit3 <- randomForest(induct ~ tIPouts, data=trainPitch)

# Using results of training, predict induct in the test set, save accuracy in pitchof_results table
pitchhof_results <- bind_rows(pitchhof_results,
  data_frame(model="Total Outs Pitched",  
  Accuracy=confusionMatrix(predict(fit3, testPitch), testPitch$induct)$overall["Accuracy"]))

# use randomForest with total wins and total innings pitched predictors   
set.seed(1)  
fit4 <- randomForest(induct ~ tW + tIPouts, data=trainPitch)

# Using results of training, predict induct in the test set, save accuracy in pitchof_results table
pitchhof_results <- bind_rows(pitchhof_results,
  data_frame(model="Total Wins and Total Outs Pitched",  
  Accuracy=confusionMatrix(predict(fit4, testPitch), testPitch$induct)$overall["Accuracy"]))

# print accuracy results of the models
pitchhof_results %>% knitr::kable()
  
# the total wins and total innings pitched model had the best results, tune this model  
bestparm <- seq(1, 2, 1) 

# tune mtry 
accuracy <- sapply(bestparm,function(sz) {
 set.seed(1) 
 fitbest <- randomForest(induct ~ tW + tIPouts, mtry=sz, data=trainPitch)
 accuracy=confusionMatrix(predict(fitbest, testPitch), testPitch$induct)$overall["Accuracy"]
} )

# accuracy is less than previous max, use default mtry
bestmtry = bestparm[which.max(accuracy)]
bestmtry
max(accuracy)

# tune ntree
bestparm <- seq(1, 500, 1) 
accuracy <- sapply(bestparm,function(sz) {
 set.seed(1) 
 fitbest <- randomForest(induct ~ tW + tIPouts, ntree=sz, data=trainPitch)
 accuracy=confusionMatrix(predict(fitbest, testPitch), testPitch$induct)$overall["Accuracy"]
} )

# superior accuracy to using default for ntree, use tuned ntree for better results, ntree is 24
bestntree = bestparm[which.max(accuracy)]
bestntree
max(accuracy)

# tune maxnodes
bestparm <- seq(10,200,10) 

accuracy <- sapply(bestparm,function(sz) {
 set.seed(1) 
 fitsz <- randomForest(induct ~ tW + tWP +tSV, maxnodes=sz, data=trainPitch)
 accuracy=confusionMatrix(predict(fitsz, testPitch), testPitch$induct)$overall["Accuracy"]
} )

# accuracy is less than previous max, use default mtry
bestmax = bestparm[which.max(accuracy)]
bestmax
max(accuracy)

# run total wins and total innings pitched model using best ntree value 
set.seed(1)  
fit5 <- randomForest(induct ~ tW + tIPouts, ntree=bestntree, data=trainPitch)

# Using results of training, predict induct in the test set, save accuracy in pitchof_results table
pitchhof_results <- bind_rows(pitchhof_results,
  data_frame(model="Total Wins and Total Outs Pitched with tuned ntree ",  
  Accuracy=confusionMatrix(predict(fit5, testPitch), testPitch$induct)$overall["Accuracy"]))

# tuned total wins and total outs pitched model produces highest accuracy
pitchhof_results %>% knitr::kable()
 
# create a list of pitchers for whom the algorithm predicted induct incorrectly
# it will be discussed in the conclusion section
y_hat <- predict(fit5, testPitch)
indx <- (y_hat != testPitch$induct)
predErr <- testPitch[indx,]
predErr <- inner_join(predErr, Master, by="playerID")
errList <- select(predErr, c(nameLast,nameFirst, finalGame, induct))
print(errList, n=100)
