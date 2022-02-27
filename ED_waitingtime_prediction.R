# INSY 672 Group Assignment - ED Waiting Time Forecasting Competition

# Team 2: Michael Church Carson and Chelsea Hon

#### THE CODE BELOW SHOWS HOW WE DEVELOPED OUR MODEL USING THE Case3Data FILE. WE HAVE COMMENTED ALL OF IT OUT SO THAT THE 
#### GRADER DOES NOT HAVE TO RUN IT AGAIN, BUT CAN STILL SEE HOW WE WORKED THROUGH THIS CHALLENGE. 
#### IF YOU RUN THIS FILE, THE ONLY SECTION THAT WILL RUN IS THE FINAL SECTION CALLED: "TEST PREDICTIVE MODEL WITH Case3Test".

# # Import Data ----
# 
# library("readxl")
# # import the Case3Data as a dataframe and ensure that each of the nine columns has the proper data type:
# Case3Data = read_excel('Case3Data.xlsm', col_types=c("date", "numeric", "text", "text", "numeric", "text", "numeric", "text", "numeric"))
# 
# # Install and Load Packages ----
# #install.packages(gbm')
# #install.packages('dplyr')
# #install.packages('randomForest')
# library(gbm)
# library(dplyr)
# library(randomForest)
# 
# Case3Data=na.omit(Case3Data)  # omit rows with missing values. We find that there are no missing values in the dataset, 
# # so we do not need to impute any values.

############################# FEATURE ENGINEERING -----------------------------------------------------------------------------

# # Change the Gender variable into a binary varible with a value of 1 if the patient is female, and 0 if the patient is a male.
# Case3Data$GenderNew = ifelse(Case3Data$Gender == 'F', 1, 0)
# Case3Data = subset(Case3Data, select = -c(Gender))
# 
# # Extract year, month, day information
# # The wait time is giong to different depending on the month, day, and hour of the day. So we need predictor variables for each.
# 
# #install.packages('lubridate')
# library(lubridate)
# Case3Data$month<-month(Case3Data$Startdatetime)
# Case3Data$day<-day(Case3Data$Startdatetime)
# Case3Data$wday<-wday(Case3Data$Startdatetime)
# Case3Data$hour<-hour(Case3Data$Startdatetime)
# 
# # Write a function to count waiting census or ED census, where "timestamp1" and "timestamp2" are columns, "time" is the 
# # Time for the census to be counted
# countnumber = function(time,timestamps1,timestamps2){
#     indices = which(time > timestamps1 & time < timestamps2)
#     return(length(indices))
# }  
# # Compute Waiting times (min) for the training data
# Case3Data$SeeMDTime=as.POSIXct(Case3Data$Startdatetime+Case3Data$Time.to.MD..Min.*60) 
# 
# # Compute ED LOS (min) for the test data
# Case3Data$DispositionTime=as.POSIXct(Case3Data$Startdatetime+Case3Data$Start.to.Disposition..Min.*60) 
# 
# for(i in 1:nrow(Case3Data)){
#     
#     # Compute waiting census when a patient A arrived at the ED. To do that, we count the number of patients whose arrival time is earlier than Patient A's arrival time and whose seeMD time is later than patient A's arrival time.
#     Case3Data$waitingcensus[i] <- countnumber(Case3Data$Startdatetime[i],Case3Data$Startdatetime,Case3Data$SeeMDTime) 
#     
#     # Compute ED census when a patient A arrived at the ED. To do that, we count the number of patients whose arrival time is earlier than Patient A's arrival time and whose disposition time is later than patient A's arrival time.
#     Case3Data$EDcensus[i] <- countnumber(Case3Data$Startdatetime[i],Case3Data$Startdatetime,Case3Data$DispositionTime)
#     
# }
# 
# # Create a interation term between Age and Triage.Acute.Score that we can experiment with includeing it in models.
# Case3Data$Age_TAS = Case3Data$Age*Case3Data$Triage.Acute.Score
# 
# # Create a interation term between waitingcensus and EDcensus that we can experiment with includeing it in models.
# Case3Data$waiting_ED = Case3Data$waitingcensus*Case3Data$EDcensus
# 
# # Create a interation term between EDcensus and Triage.Acute.Score that we can experiment with includeing  it in models.
# Case3Data$ED_TAS = Case3Data$EDcensus*Case3Data$Triage.Acute.Score
# 

################ MODELS -----------------------------------------------------------------------------------------------------

# ## Simple Linear Regressions
# # Model 1 - Simple Linear Regression
# lm_reg=lm(Time.to.MD..Min.~Age+Triage.Acute.Score+GenderNew+waitingcensus+EDcensus, data=Case3Data) 
# predictedvalues=predict(lm_reg,Case3Data) #use your trained model lm_reg to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error #display the RMSE.
# # RMSE = 7.396217
# 
# # Model 2 - Simple Linear Regression wuth an interaction term
# lm_reg=lm(Time.to.MD..Min.~Age+factor(Arrival.Model)+factor(Chief.Complain.System)+Triage.Acute.Score+GenderNew+waitingcensus+EDcensus+Age_TAS, data=Case3Data) 
# predictedvalues=predict(lm_reg,Case3Data) #use your trained model lm_reg to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 7.3933
# # Finding: Interatcion term between Age and Triage.Acute.Score improves RMSE slightly.
# 
# # Model 3 -  Simple Linear Regression with an interaction term
# lm_reg=lm(Time.to.MD..Min.~Age+factor(Arrival.Model)+factor(Chief.Complain.System)+Triage.Acute.Score+GenderNew+waitingcensus+EDcensus+waiting_ED, data=Case3Data) 
# predictedvalues=predict(lm_reg,Case3Data) #use your trained model lm_reg to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 7.395253
# # Finding: Interatcion term between waitingcensus and EDcensus does not improve RMSE.
# 
# # Model 4 - Simple Linear Regression with an interaction term
# lm_reg=lm(Time.to.MD..Min.~Age+factor(Arrival.Model)+factor(Chief.Complain.System)+Triage.Acute.Score+GenderNew+waitingcensus+EDcensus+ED_TAS, data=Case3Data) 
# predictedvalues=predict(lm_reg,Case3Data) #use your trained model lm_reg to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 7.396202
# # Finding: Interatcion term between EDcensus and Triage.Acute.Score does not improve RMSE.
# 
# # Only the interaction term between Age and Triage.Acute.Score improves the predictive abililty (lowering RMSE) of our models, 
# # at least when we are using simple linear regression. Next, we will experiment with more complex forms of models.
# 
# # Model 5 - Random Forest with 100 trees
# myforest=randomForest(Time.to.MD..Min.~
#                           Age+
#                           Arrival.Model+
#                           Chief.Complain.System+
#                           Triage.Acute.Score+
#                           GenderNew+
#                           waitingcensus+
#                           EDcensus+
#                           Age_TAS, ntree=100, data=Case3Data, importance=TRUE, do.trace =10)
# 
# myforest
# 
# predictedvalues=predict(myforest,Case3Data) #use your trained model myforest to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# RMSE = 4.608186
# Finding: The Random Forest has much better RMSE than the simple linear regressions, even when we only use 100 trees. The results of the 
# random forest indicate that RMSE lowers as more trees are added, so we likely could improve predictive power even more by adding more
# trees.

# # Model 6 - Random Forest with 500 trees
# myforest=randomForest(Time.to.MD..Min.~
#                           Age+
#                           Arrival.Model+
#                           Chief.Complain.System+
#                           Triage.Acute.Score+
#                           GenderNew+
#                           waitingcensus+
#                           EDcensus+
#                           Age_TAS, ntree=500, data=Case3Data, importance=TRUE, do.trace =50)
# 
# myforest
# 
# predictedvalues=predict(myforest,Case3Data) #use your trained model myforest to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 4.599693
# # Finding: Increasing the number of trees to 500 only marginally improves RMSE. 
# 
# # Model 7 - Random Forest with 100 trees and two interation terms: Age_TAS and waiting_ED
# myforest=randomForest(Time.to.MD..Min.~
#                           Age+
#                           Arrival.Model+
#                           Chief.Complain.System+
#                           Triage.Acute.Score+
#                           GenderNew+
#                           waitingcensus+
#                           EDcensus+
#                           Age_TAS+
#                           waiting_ED, ntree=100, data=Case3Data, importance=TRUE, do.trace =10)
# 
# myforest
# 
# predictedvalues=predict(myforest,Case3Data) #use your trained model myforest to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 3.646962
# 
# # Model 8 - Random Forest with 100 trees and three interation terms: Age_TAS, waiting_ED, and ED_TAS
# 
# myforest=randomForest(Time.to.MD..Min.~
#                           Age+
#                           Arrival.Model+
#                           Chief.Complain.System+
#                           Triage.Acute.Score+
#                           GenderNew+
#                           waitingcensus+
#                           EDcensus+
#                           Age_TAS+
#                           waiting_ED+
#                           ED_TAS, ntree=100, data=Case3Data, importance=TRUE, do.trace =10)
# 
# myforest
# 
# predictedvalues=predict(myforest,Case3Data) #use your trained model myforest to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 3.688056
# 
# # Model 9 - Gradiant Boosted Machine with one interation term: Age_TAS
# 
# # Set random seed so that results are reproducible
# set.seed (1)
# 
# boosted=gbm(Time.to.MD..Min.~
#                 Age+
#                 factor(Arrival.Model)+
#                 factor(Chief.Complain.System)+
#                 Triage.Acute.Score+
#                 GenderNew+
#                 waitingcensus+
#                 EDcensus+
#                 Age_TAS,
#             data=Case3Data,distribution="gaussian",n.trees=10000, interaction.depth=6)
# 
# # Calculate error
# predictedvalues=predict(boosted,Case3Data) #use your trained model lm_reg to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 5.553786
# 
# # Model 10 - Gradiant Boosted Machine with two interation terms: Age_TAS and waiting_ED
# 
# boosted=gbm(Time.to.MD..Min.~
#                 Age+
#                 factor(Arrival.Model)+
#                 factor(Chief.Complain.System)+
#                 Triage.Acute.Score+
#                 GenderNew+
#                 waitingcensus+
#                 EDcensus+
#                 Age_TAS +
#                 waiting_ED,
#             data=Case3Data,distribution="gaussian",n.trees=10000, interaction.depth=6)
# 
# # Calculate error
# predictedvalues=predict(boosted,Case3Data) #use your trained model lm_reg to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-Case3Data$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# RMSE = 5.382011

# Let's go back to feature engineering to try to improve our models.

# #### MORE FEATURE ENGINEERING -------------------------------------------------------------------------------------------------
# 
# # Dummify Arrival.Model & Chief.Complain.System and remove all space in column names
# # install.packages("fastDummies")
# # install.packages('stringr')
# library(stringr)
# require(fastDummies)
# Case3Data <- Case3Data %>% mutate(Arrival.Model = str_replace_all(Arrival.Model, "\\(|\\)", ""))
# Case3Data$Arrival.Model<-gsub(" ", "", Case3Data$Arrival.Model)
# Case3Data$Arrival.Model<-gsub(",", "", Case3Data$Arrival.Model)
# Case3Data$Arrival.Model<-gsub("&", "", Case3Data$Arrival.Model)
# Case3Data$Arrival.Model<-gsub("-", "_", Case3Data$Arrival.Model)
# Case3Data = dummy_cols(Case3Data, select_columns = c('Arrival.Model'),
#                        remove_first_dummy = TRUE,
#                        remove_selected_columns = TRUE)
# 
# Case3Data$Chief.Complain.System<-gsub(" ", "", Case3Data$Chief.Complain.System)
# Case3Data$Chief.Complain.System<-gsub(",", "", Case3Data$Chief.Complain.System)
# Case3Data$Chief.Complain.System<-gsub("&", "", Case3Data$Chief.Complain.System)
# Case3Data$Chief.Complain.System<-gsub("-", "_", Case3Data$Chief.Complain.System)
# Case3Data = dummy_cols(Case3Data, select_columns = c('Chief.Complain.System'),
#                        remove_first_dummy = TRUE,
#                        remove_selected_columns = TRUE)
# 
# # Also dummify the following
# Case3Data = dummy_cols(Case3Data, select_columns = c('Triage.Acute.Score'),
#                        remove_first_dummy = TRUE,
#                        remove_selected_columns = TRUE)
# 
# # Drop information released after the start time of the patient which ED WT is to be predicted
# df <- select(Case3Data, -c("SeeMDTime","DispositionTime","Discharged Home","Start.to.Disposition..Min."))
# 
# 
# # We have found that random forest models perform the best, so we will try to optimize our random forest models further. To improve
# # our models further we will now take a systematic approach to selecting features.
# 
# # Let's first create a baseline for the features we include in each model.
# 
# # FEATURE SELECTION ------------------------------------------------------------------------------------------------------------
# 
# # Create empty list for cumulative predictors which improves adjusted r2
# Rsquares = list()
# Pvalues = list()
# MregAdjRsquares = list()
# MregPvalues = list()
# var = list()
# decision = list()
# bestrsquared = 0
# 
# df <- df %>%
#     select(Time.to.MD..Min., everything())
# predictors = colnames(df)
# predictors = predictors[-1]
# predictors = as.list(predictors)
# 
# attach(df)
# 
# for(i in 2:ncol(df)){
#     testlm1 = lm(Time.to.MD..Min.~unlist(df[,i]))
#     rsquared = summary(testlm1)$r.squared
#     pvalue = summary(testlm1)$coefficients[ , 4]
#     Rsquares = append(Rsquares, rsquared)
#     Pvalues = append(Pvalues, pvalue)
#     
#     # Add new predictor to rolling list var, run multiple linear regression, and get adjusted r2 and p-value
#     if(length(var)==0){
#         f = as.formula(paste('Time.to.MD..Min.',unlist(predictors[i-1]),sep = '~'))
#     } else{
#         f = as.formula(paste('Time.to.MD..Min.',
#                              paste(paste(unlist(var), collapse= '+'),
#                                    unlist(predictors[i-1]),
#                                    sep= '+'),
#                              sep = '~'))
#     }
#     testlm2 = lm(f)
#     mregAdjRsquares = summary(testlm2)$adj.r.squared
#     mregPvalues = summary(testlm2)$coefficients[ , 4]
#     MregAdjRsquares = append(MregAdjRsquares, mregAdjRsquares)
#     MregPvalues = append(MregPvalues, mregPvalues)
#     
#     ## Test if adding new predictor to rolling list var can improve adjusted r2
#     ## If r2 increases keep, otherwise drop and move on
#     if (mregAdjRsquares > bestrsquared){
#         var = append(var,predictors[i-1])
#         bestrsquared = summary(testlm2)$adj.r.squared
#         decision = append(decision,"keep")
#     } else{
#         decision = append(decision,"drop")
#     }
# }
# 
# # Write a single linear regression results and multiple linear regression results to csv
# results= do.call(rbind, Map(data.frame, Predictor = predictors, R_Squared=Rsquares, P_Value=Pvalues, MregAdjRsquares=MregAdjRsquares, MregPvalues=MregPvalues, Decision=decision))
# write.csv(results[1:ncol(df)-1,],"result.csv", row.names = TRUE)
# print(bestrsquared)
# print(length(var))
# ## highest adjusted r2 = 0.8568227, by keeping 21 predictors
# 
# ## Now we will use the features identified by the above feature selection process to create more models.
# 
# # Model 11: Simple linear regression with predictors identified 
# lm_reg=lm(Time.to.MD..Min.~
#               Age+
#               day+
#               wday+
#               hour+
#               waitingcensus+
#               EDcensus+
#               Arrival.Model_GroundAmbulance+
#               Chief.Complain.System_ENT_MouthThroatNeck+
#               Chief.Complain.System_ENT_Nose+
#               Chief.Complain.System_Gastrointestinal+
#               Chief.Complain.System_GeneralMinor+
#               Chief.Complain.System_Genitourinary+
#               Chief.Complain.System_MentalHealth+
#               Chief.Complain.System_Neurologic+
#               Chief.Complain.System_Ophthalmology+
#               Chief.Complain.System_Skin+
#               Chief.Complain.System_SUBSTANCEMISUSE+
#               Triage.Acute.Score_2+
#               Triage.Acute.Score_3+
#               Triage.Acute.Score_4+
#               Triage.Acute.Score_5,
#           data=df) 
# predictedvalues=predict(lm_reg,df) #use your trained model lm_reg to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-df$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error #display the RMSE.
# # RMSE = 7.315148
# # This is the best linear model yet, so our feature selection process was successful. 
# 
# # Model 12: Random Forest with 100 trees and predictors identified 
# set.seed(5)
# myforest=randomForest(Time.to.MD..Min.~
#                           Age+
#                           day+
#                           wday+
#                           hour+
#                           waitingcensus+
#                           EDcensus+
#                           Arrival.Model_GroundAmbulance+
#                           Chief.Complain.System_ENT_MouthThroatNeck+
#                           Chief.Complain.System_ENT_Nose+
#                           Chief.Complain.System_Gastrointestinal+
#                           Chief.Complain.System_GeneralMinor+
#                           Chief.Complain.System_Genitourinary+
#                           Chief.Complain.System_MentalHealth+
#                           Chief.Complain.System_Neurologic+
#                           Chief.Complain.System_Ophthalmology+
#                           Chief.Complain.System_Skin+
#                           Chief.Complain.System_SUBSTANCEMISUSE+
#                           Triage.Acute.Score_2+
#                           Triage.Acute.Score_3+
#                           Triage.Acute.Score_4+
#                           Triage.Acute.Score_5,
#                       ntree=100, data=df, importance=TRUE, do.trace =10)
# 
# myforest
# 
# predictedvalues=predict(myforest,df) #use your trained model myforest to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-df$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 2.865364
# 
# # 2.865 is a new low, but we think it can be reduced even further if we do some more feature engineering:
# 
# # Create a copy of the dataset
# Case3Data2 <- Case3Data
# 
# # Create a interation term between waitingcensus and EDcensus that we can experiment with includeing it in models.
# Case3Data2$waiting_ED = Case3Data2$waitingcensus * Case3Data2$EDcensus
# 
# # Create a interation term between EDcensus and Triage.Acute.Score that we can experiment with includeing  it in models.
# Case3Data2$ED_TAS2 = Case3Data2$EDcensus*Case3Data2$Triage.Acute.Score_2
# Case3Data2$ED_TAS3 = Case3Data2$EDcensus*Case3Data2$Triage.Acute.Score_3
# Case3Data2$ED_TAS4 = Case3Data2$EDcensus*Case3Data2$Triage.Acute.Score_4
# Case3Data2$ED_TAS5 = Case3Data2$EDcensus*Case3Data2$Triage.Acute.Score_5
# 
# # Drop information released after the start time of the patient which ED WT is to be predicted
# df2 <- select(Case3Data2, -c("SeeMDTime","DispositionTime","Discharged Home","Start.to.Disposition..Min."))
# 
# # Re-run feature selection process ----
# 
# # Create empty list for cumulative predictors which improves adjusted r2
# Rsquares = list()
# Pvalues = list()
# MregAdjRsquares = list()
# MregPvalues = list()
# var = list()
# decision = list()
# bestrsquared = 0
# 
# df2 <- df2 %>%
#     select(Time.to.MD..Min., everything())
# predictors = colnames(df2)
# predictors = predictors[-1]
# predictors = as.list(predictors)
# 
# attach(df2)
# 
# for(i in 2:ncol(df2)){
#     testlm1 = lm(Time.to.MD..Min.~unlist(df2[,i]))
#     rsquared = summary(testlm1)$r.squared
#     pvalue = summary(testlm1)$coefficients[ , 4]
#     Rsquares = append(Rsquares, rsquared)
#     Pvalues = append(Pvalues, pvalue)
#     
#     # Add new predictor to rolling list var, run multiple linear regression, and get adjusted r2 and p-value
#     if(length(var)==0){
#         f = as.formula(paste('Time.to.MD..Min.',unlist(predictors[i-1]),sep = '~'))
#     } else{
#         f = as.formula(paste('Time.to.MD..Min.',
#                              paste(paste(unlist(var), collapse= '+'),
#                                    unlist(predictors[i-1]),
#                                    sep= '+'),
#                              sep = '~'))
#     }
#     testlm2 = lm(f)
#     mregAdjRsquares = summary(testlm2)$adj.r.squared
#     mregPvalues = summary(testlm2)$coefficients[ , 4]
#     MregAdjRsquares = append(MregAdjRsquares, mregAdjRsquares)
#     MregPvalues = append(MregPvalues, mregPvalues)
#     
#     ## Test if adding new predictor to rolling list var can improve adjusted r2
#     ## If r2 increases keep, otherwise drop and move on
#     if (mregAdjRsquares > bestrsquared){
#         var = append(var,predictors[i-1])
#         bestrsquared = summary(testlm2)$adj.r.squared
#         decision = append(decision,"keep")
#     } else{
#         decision = append(decision,"drop")
#     }
# }
# 
# # Write single linear regression results and multiple linear regression results to csv
# results= do.call(rbind, Map(data.frame, Predictor = predictors, R_Squared=Rsquares, P_Value=Pvalues, MregAdjRsquares=MregAdjRsquares, MregPvalues=MregPvalues, Decision=decision))
# write.csv(results[1:ncol(df2)-1,],"result2.csv", row.names = TRUE)
# print(bestrsquared)
# print(length(var))
# ## highest adjusted r2 = 0.8598501, by keeping 26 predictors
# 
# # Model 13: Random Forest with 100 trees and the 26 predictors identified 
# set.seed(5)
# myforest=randomForest(Time.to.MD..Min.~
#                           Age+
#                           day+
#                           wday+
#                           hour+
#                           waitingcensus+
#                           EDcensus+
#                           Arrival.Model_GroundAmbulance+
#                           Chief.Complain.System_ENT_MouthThroatNeck+
#                           Chief.Complain.System_ENT_Nose+
#                           Chief.Complain.System_Gastrointestinal+
#                           Chief.Complain.System_GeneralMinor+
#                           Chief.Complain.System_Genitourinary+
#                           Chief.Complain.System_MentalHealth+
#                           Chief.Complain.System_Neurologic+
#                           Chief.Complain.System_Ophthalmology+
#                           Chief.Complain.System_SUBSTANCEMISUSE+
#                           Chief.Complain.System_Trauma+
#                           Triage.Acute.Score_2+
#                           Triage.Acute.Score_3+
#                           Triage.Acute.Score_4+
#                           Triage.Acute.Score_5+
#                           waiting_ED+
#                           ED_TAS2+
#                           ED_TAS3+
#                           ED_TAS4,
#                       ntree=100, data=df2, importance=TRUE, do.trace =10)
# 
# myforest
# 
# predictedvalues=predict(myforest,df2) #use your trained model myforest to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-df$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# 
# # Now run previous model for 500 trees, as we still have plenty of training time considering the 12 hours given

# # Model 14: Random Forest with 500 trees and predictors identified 
# set.seed(5)
# myforest=randomForest(Time.to.MD..Min.~
#                           Age+
#                           day+
#                           wday+
#                           hour+
#                           waitingcensus+
#                           EDcensus+
#                           Arrival.Model_GroundAmbulance+
#                           Chief.Complain.System_ENT_MouthThroatNeck+
#                           Chief.Complain.System_ENT_Nose+
#                           Chief.Complain.System_Gastrointestinal+
#                           Chief.Complain.System_GeneralMinor+
#                           Chief.Complain.System_Genitourinary+
#                           Chief.Complain.System_MentalHealth+
#                           Chief.Complain.System_Neurologic+
#                           Chief.Complain.System_Ophthalmology+
#                           Chief.Complain.System_SUBSTANCEMISUSE+
#                           Chief.Complain.System_Trauma+
#                           Triage.Acute.Score_2+
#                           Triage.Acute.Score_3+
#                           Triage.Acute.Score_4+
#                           Triage.Acute.Score_5+
#                           waiting_ED+
#                           ED_TAS2+
#                           ED_TAS3+
#                           ED_TAS4,
#                       ntree=500, data=df2, importance=TRUE, do.trace =10)
# 
# myforest
# 
# predictedvalues=predict(myforest,df2) #use your trained model myforest to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-df$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# 
# # 
# 
# # We hypothesize that patients who arrive during off-business hours will create more pressure on the ED because there 
# # will be less staff at the hospital. Let's create a an off-business feature to add to the model.
# 
# # off-business refers to weekdays after 4pm and weekends
# Case3Data3 <- Case3Data2
# 
# Case3Data3$offbusiness = ifelse(Case3Data$wday >5 | ((Case3Data$wday <=5) && (Case3Data$hour > 4)), 1, 0)
# 
# df3 <- select(Case3Data3, -c("SeeMDTime","DispositionTime","Discharged Home","Start.to.Disposition..Min."))
# df3 <- df3 %>%
#     select(Time.to.MD..Min., everything())
# attach(df3)
# 
# # Model 15: Random Forest with 100 trees and predictors identified 
# set.seed(5)
# myforest=randomForest(Time.to.MD..Min.~
#                           Age+
#                           day+
#                           wday+
#                           hour+
#                           waitingcensus+
#                           EDcensus+
#                           Arrival.Model_GroundAmbulance+
#                           Chief.Complain.System_ENT_MouthThroatNeck+
#                           Chief.Complain.System_ENT_Nose+
#                           Chief.Complain.System_Gastrointestinal+
#                           Chief.Complain.System_GeneralMinor+
#                           Chief.Complain.System_Genitourinary+
#                           Chief.Complain.System_MentalHealth+
#                           Chief.Complain.System_Neurologic+
#                           Chief.Complain.System_Ophthalmology+
#                           Chief.Complain.System_SUBSTANCEMISUSE+
#                           Chief.Complain.System_Trauma+
#                           Triage.Acute.Score_2+
#                           Triage.Acute.Score_3+
#                           Triage.Acute.Score_4+
#                           Triage.Acute.Score_5+
#                           waiting_ED+
#                           ED_TAS2+
#                           ED_TAS3+
#                           ED_TAS4+
#                           offbusiness,
#                       ntree=100, data=df3, importance=TRUE, do.trace =10)
# 
# myforest
# 
# predictedvalues=predict(myforest,df3) #use your trained model myforest to predict the ED WT using attributes in test data
# test_error<-sqrt(mean((predictedvalues-df$Time.to.MD..Min.)^2)) #compare the predicted values to the actual values and calculate the RMSE
# test_error
# # RMSE = 2.756896 


#==================================================================================================================================#
##### TEST PREDICTIVE MODEL WITH Case3Test -----------------------------------------------------------------------------------------

# Import Data ----

library("readxl")
# import the Case3Data as a dataframe and ensure that each of the nine columns has the proper data type:
Case3Test = read_excel('Case3Test.xlsm', col_types=c("date", "numeric", "text", "text", "numeric", "text", "numeric", "text", "numeric"))

# Install and Load Packages ----
#install.packages(gbm')
#install.packages('dplyr')
#install.packages('randomForest')
library(gbm)
library(dplyr)
library(randomForest)

Case3Test=na.omit(Case3Test)  # omit rows with missing values. We find that there are no missing values in the dataset, 
# so we do not need to impute any values.

############################# FEATURE ENGINEERING -----------------------------------------------------------------------------

# Change the Gender variable into a binary varible with a value of 1 if the patient is female, and 0 if the patient is a male.
Case3Test$GenderNew = ifelse(Case3Test$Gender == 'F', 1, 0)
Case3Test = subset(Case3Test, select = -c(Gender))

# Extract year, month, day information
# The wait time is giong to different depending on the month, day, and hour of the day. So we need predictor variables for each.

#install.packages('lubridate')
library(lubridate)
Case3Test$month<-month(Case3Test$Startdatetime)
Case3Test$day<-day(Case3Test$Startdatetime)
Case3Test$wday<-wday(Case3Test$Startdatetime)
Case3Test$hour<-hour(Case3Test$Startdatetime)

# Write a function to count waiting census or ED census, where "timestamp1" and "timestamp2" are columns, "time" is the 
# Time for the census to be counted
countnumber = function(time,timestamps1,timestamps2){
    indices = which(time > timestamps1 & time < timestamps2)
    return(length(indices))
}  
# Compute Waiting times (min) for the training data
Case3Test$SeeMDTime=as.POSIXct(Case3Test$Startdatetime+Case3Test$Time.to.MD..Min.*60) 

# Compute ED LOS (min) for the test data
Case3Test$DispositionTime=as.POSIXct(Case3Test$Startdatetime+Case3Test$Start.to.Disposition..Min.*60) 

for(i in 1:nrow(Case3Test)){
    
    # Compute waiting census when a patient A arrived at the ED. To do that, we count the number of patients whose arrival time is earlier than Patient A's arrival time and whose seeMD time is later than patient A's arrival time.
    Case3Test$waitingcensus[i] <- countnumber(Case3Test$Startdatetime[i],Case3Test$Startdatetime,Case3Test$SeeMDTime) 
    
    # Compute ED census when a patient A arrived at the ED. To do that, we count the number of patients whose arrival time is earlier than Patient A's arrival time and whose disposition time is later than patient A's arrival time.
    Case3Test$EDcensus[i] <- countnumber(Case3Test$Startdatetime[i],Case3Test$Startdatetime,Case3Test$DispositionTime)
    
}

# Dummify Arrival.Model & Chief.Complain.System and remove all space in column names

# install.packages("fastDummies")
# install.packages('stringr')
library(stringr)
require(fastDummies)
Case3Test <- Case3Test %>% mutate(Arrival.Model = str_replace_all(Arrival.Model, "\\(|\\)", ""))
Case3Test$Arrival.Model<-gsub(" ", "", Case3Test$Arrival.Model)
Case3Test$Arrival.Model<-gsub(",", "", Case3Test$Arrival.Model)
Case3Test$Arrival.Model<-gsub("&", "", Case3Test$Arrival.Model)
Case3Test$Arrival.Model<-gsub("-", "_", Case3Test$Arrival.Model)
Case3Test = dummy_cols(Case3Test, select_columns = c('Arrival.Model'),
                       remove_first_dummy = TRUE,
                       remove_selected_columns = TRUE)

Case3Test$Chief.Complain.System<-gsub(" ", "", Case3Test$Chief.Complain.System)
Case3Test$Chief.Complain.System<-gsub(",", "", Case3Test$Chief.Complain.System)
Case3Test$Chief.Complain.System<-gsub("&", "", Case3Test$Chief.Complain.System)
Case3Test$Chief.Complain.System<-gsub("-", "_", Case3Test$Chief.Complain.System)
Case3Test = dummy_cols(Case3Test, select_columns = c('Chief.Complain.System'),
                       remove_first_dummy = TRUE,
                       remove_selected_columns = TRUE)

# Also dummify the following
Case3Test = dummy_cols(Case3Test, select_columns = c('Triage.Acute.Score'),
                       remove_first_dummy = TRUE,
                       remove_selected_columns = TRUE)

# Create a interation term between waitingcensus and EDcensus that we can experiment with includeing it in models.
Case3Test$waiting_ED = Case3Test$waitingcensus*Case3Test$EDcensus

# Create a interation term between Age and Triage.Acute.Score that we can experiment with includeing it in models.
Case3Test$ED_TAS2 = Case3Test$EDcensus*Case3Test$Triage.Acute.Score_2
Case3Test$ED_TAS3 = Case3Test$EDcensus*Case3Test$Triage.Acute.Score_3
Case3Test$ED_TAS4 = Case3Test$EDcensus*Case3Test$Triage.Acute.Score_4
Case3Test$ED_TAS5 = Case3Test$EDcensus*Case3Test$Triage.Acute.Score_5

# Create off-business flag to indicate potential rush of weekdays after 4pm and weekends
Case3Test$offbusiness = ifelse(Case3Test$wday >5 | ((Case3Test$wday <=5) && (Case3Test$hour > 4)), 1, 0)

#Drop information released after the start time of the patient which ED WT is to be predicted
df <- select(Case3Test, -c("SeeMDTime","DispositionTime","Discharged Home","Start.to.Disposition..Min."))

# We have found that random forest models perform the best, so we will try to optimize our random forest models further. To improve
# our models further we will now take a systematic approach to selecting features.

# FEATURE SELECTION ------------------------------------------------------------------------------------------------------------

# Create empty list for cumulative predictors which improves adjusted r2
Rsquares = list()
Pvalues = list()
MregAdjRsquares = list()
MregPvalues = list()
var = list()
decision = list()
bestrsquared = 0

df <- df %>%
    select(Time.to.MD..Min., everything())
predictors = colnames(df)
predictors = predictors[-1]
predictors = as.list(predictors)

attach(df)

for(i in 2:ncol(df)){
    testlm1 = lm(Time.to.MD..Min.~unlist(df[,i]))
    rsquared = summary(testlm1)$r.squared
    pvalue = summary(testlm1)$coefficients[ , 4]
    Rsquares = append(Rsquares, rsquared)
    Pvalues = append(Pvalues, pvalue)
    
    # Add new predictor to rolling list var, run multiple linear regression, and get adjusted r2 and p-value
    if(length(var)==0){
        f = as.formula(paste('Time.to.MD..Min.',unlist(predictors[i-1]),sep = '~'))
    } else{
        f = as.formula(paste('Time.to.MD..Min.',
                             paste(paste(unlist(var), collapse= '+'),
                                   unlist(predictors[i-1]),
                                   sep= '+'),
                             sep = '~'))
    }
    testlm2 = lm(f)
    mregAdjRsquares = summary(testlm2)$adj.r.squared
    mregPvalues = summary(testlm2)$coefficients[ , 4]
    MregAdjRsquares = append(MregAdjRsquares, mregAdjRsquares)
    MregPvalues = append(MregPvalues, mregPvalues)
    
    ## Test if adding new predictor to rolling list var can improve adjusted r2
    ## If r2 increases keep, otherwise drop and move on
    if (mregAdjRsquares > bestrsquared){
        var = append(var,predictors[i-1])
        bestrsquared = summary(testlm2)$adj.r.squared
        decision = append(decision,"keep")
    } else{
        decision = append(decision,"drop")
    }
}

# Write predictors selection to csv
results= do.call(rbind, Map(data.frame, Predictor = predictors, R_Squared=Rsquares, P_Value=Pvalues, MregAdjRsquares=MregAdjRsquares, MregPvalues=MregPvalues, Decision=decision))
#write.csv(results[1:ncol(df)-1,],"result.csv", row.names = TRUE)
print(bestrsquared)
print(length(var))
## highest adjusted r2 = 0.8578082, by keeping 27 predictors

# FINAL MODEL ####################################################################################################################
# Model 15: Random Forest with 500 trees and predictors identified 
# Note: This model took us approximately 45 minutes to run.
set.seed(5)
myforest=randomForest(Time.to.MD..Min.~
                          Age+
                          day+
                          wday+
                          hour+
                          waitingcensus+
                          EDcensus+
                          Arrival.Model_GroundAmbulance+
                          Chief.Complain.System_ENT_MouthThroatNeck+
                          Chief.Complain.System_ENT_Nose+
                          Chief.Complain.System_Gastrointestinal+
                          Chief.Complain.System_GeneralMinor+
                          Chief.Complain.System_Genitourinary+
                          Chief.Complain.System_MentalHealth+
                          Chief.Complain.System_Neurologic+
                          Chief.Complain.System_Ophthalmology+
                          Chief.Complain.System_Skin+
                          Chief.Complain.System_SUBSTANCEMISUSE+
                          Triage.Acute.Score_2+
                          Triage.Acute.Score_3+
                          Triage.Acute.Score_4+
                          Triage.Acute.Score_5+
                          waiting_ED+
                          ED_TAS2+
                          ED_TAS3+
                          ED_TAS4+
                          ED_TAS5+
                          offbusiness,
                      ntree=500, data=df, importance=TRUE, do.trace =100)

predictedvalues=predict(myforest,df) # use your trained model myforest to predict the ED WT using attributes in test data
test_error<-sqrt(mean((predictedvalues-df$Time.to.MD..Min.)^2)) # compare the predicted values to the actual values and calculate the RMSE
cat("The RMSE of our final model is:", test_error)

