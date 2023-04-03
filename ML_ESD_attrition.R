
#MACHINE LEARNING
#install.packages("xgboost")
library(xgboost)
#install.packages("caTools")
library(caTools)
#install.packages("cvms")
library(cvms)
#install.packages("caret")
library(caret)
#install.packages("fastDummies")
library(fastDummies)
library(Matrix)
#install.packages("shapr")
library(shapr)
#install.packages("SHAPforxgboost")
library(tidyverse)
#install.packages("shapper") # If you don't have devtools installed
library(shapper)
#start with King county

newdat <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/dat_econ_of_ed.csv")
newdat$esdcode <- newdat$ESDCode
newdat$esdname <- newdat$ESDName



#now we will start creating variables according to our conversation.
#exp - year mean centered, and then make 0-5 grouping



#center first then cut
newdat <- newdat %>% mutate(expcut = case_when(
  exp <= 5 ~ exp,
  between(exp, 5.0001, 15) ~ (exp-5),
  between(exp, 15.0001, 23) ~ (exp-15),
  exp > 23 ~ exp-23
))

max(newdat$exp, na.rm = TRUE)
max(newdat$expcut, na.rm = TRUE)

#now we year-mean center
newdat <- newdat %>% group_by(schyear) %>% mutate(cent_expcut = scale(expcut, scale = FALSE)[,1])
#newdat <- newdat %>% group_by(schyear) %>% mutate(cent_expcut = scale(expcut, scale = TRUE)[,1])


#tfinsal/cins/unemp we will mean center
newdat <- newdat %>% group_by(schyear) %>% mutate(
  cent_tfinsal = scale(log(tfinsal+1), scale = FALSE)[, 1],
  cent_cins = scale(log(cins+1), scale = FALSE)[, 1],
  cent_crime = scale(RATE, scale = FALSE)[, 1],
  cent_unemp = scale(janunemp_rate, scale = FALSE)[, 1],
  cent_medhouse = scale(log(medhouse), scale = FALSE)[, 1],
  cent_medinc = scale(log(medinc), scale = FALSE)[, 1]
)

newdat <- newdat %>% group_by(schyear) %>% mutate(
  cent_tfinsal = scale(log(tfinsal+1), scale = TRUE)[, 1],
  cent_cins = scale(log(cins+1), scale = TRUE)[, 1],
  cent_crime = scale(RATE, scale = TRUE)[, 1],
  cent_unemp = scale(janunemp_rate, scale = TRUE)[, 1],
  cent_medhouse = scale(log(medhouse), scale = TRUE)[, 1],
  cent_medinc = scale(log(medinc), scale = TRUE)[, 1]
)



write.csv(newdat, "~/Downloads/INDEP/teacher_attrition_covid/dat_ML.csv")


newdat <- newdat %>% mutate(duty = case_when(
  droot == 31 ~ "Elementary Homeroom Teacher",
  droot == 32 ~ "Secondary Teacher",
  droot == 33 ~ "Other Teacher",
  TRUE ~ "Non-teacher"
))

options(scipen = 100)

table(newdat$esdname)

table(newdat$esdname)
newdatk <- newdat %>% filter(esdname == "Puget Sound Educational Service District 121", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Puget Sound Educational Service District 121", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Puget Sound Educational Service District 121", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Puget Sound Educational Service District 121", exp > 23, teacher == 1)#, droot == 33)
ml("Early Career")
ml("Mid Career")
ml("Late Career")
table(newdat$esdname)
newdatk <- newdat %>% filter(esdname == "Educational Service District 105", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 105", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 105", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 105", exp > 23, teacher == 1)#, droot == 33)

table(newdat$esdname) #crime rate is so frikin high - WHAT
newdatk <- newdat %>% filter(esdname == "Capital Region ESD 113", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Capital Region ESD 113", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Capital Region ESD 113", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Capital Region ESD 113", exp > 23, teacher == 1)#, droot == 33)


table(newdat$esdname)
newdatk <- newdat %>% filter(esdname == "Northwest Educational Service District 189", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Northwest Educational Service District 189", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Northwest Educational Service District 189", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Northwest Educational Service District 189", exp > 23, teacher == 1)#, droot == 33)

ml("Early Career")
ml("Mid Career")
ml("Late Career")

table(newdat$esdname) #third group is so confusing we can talk more about max utility
newdatk <- newdat %>% filter(esdname == "North Central Educational Service District 171", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "North Central Educational Service District 171", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "North Central Educational Service District 171", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "North Central Educational Service District 171", exp > 23, teacher == 1)#, droot == 33)

newdatk <- newdat %>% filter(esdname == "Educational Service District 101", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 101", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 101", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 101", exp > 23, teacher == 1)#, droot == 33)

newdatk <- newdat %>% filter(esdname == "Educational Service District 112", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 112", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 112", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 112", exp > 23, teacher == 1)#, droot == 33)


ml("Early Career")
ml("Mid Career")
ml("Late Career")

newdatk <- newdat %>% filter(esdname == "Educational Service District 123", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 123", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 123", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Educational Service District 123", exp > 23, teacher == 1)#, droot == 33)

#BACK
newdatk <- newdat %>% filter(esdname == "Olympic Educational Service District 114", exp <= 5, teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Olympic Educational Service District 114", between(exp, 5.0001, 15), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Olympic Educational Service District 114", between(exp, 15.0001, 23), teacher == 1)#, droot == 33)
newdatk <- newdat %>% filter(esdname == "Olympic Educational Service District 114", exp > 23, teacher == 1)#, droot == 33)

ml()
ml2()
newdat %>% group_by(schyear) %>% summarise(
  across(left_dataset, list(mean = mean, median = median))
) %>% print(n = length(unique(newdat$schyear)))

#try viz first
library(ggplot2)
library(reshape2)
prepviz <- newdat %>% ungroup() %>% select(schyear, left_dataset, esdname, esdcode)
prepviz <- prepviz %>% filter(esdname != "Spokane Public Schools Charter Authorizer", esdname != 
                                           "Office of Superintendent of Public Instruction", esdname != 
                                           "Washington State Charter School Commission", schyear < 2023)
#reshape data
prepviz <- dcast(prepviz,schyear + esdname + esdcode ~ left_dataset, value.var = "left_dataset")
colnames(prepviz) <- c("schyear", "esdname", "esdcode", "stay", "leave")


prepviz <- prepviz %>% mutate(att_rate = leave/(stay + leave))
str(prepviz)
#prepviz$schyear <- as.factor(prepviz$schyear)


ggplot(data = prepviz, aes(x = schyear, y = att_rate, color = esdname)) +
  geom_line() + ylim(0, 0.18) +
  labs(x = "School Year", y = "Attrition Rate", color = "Educational Service District (ESD)") +
  theme_bw() + scale_x_continuous(breaks = seq(2018, 2022, 1)) + ggtitle("Teacher attrition rate between school year 2017 - 2018 to 2021 - 2022 by ESD")


ml <- function(group) {
##start ML
var <- c("sex", "cent_cins", "cent_tfinsal", "cent_expcut", "hispanic", "cent_medhouse", "perc_white",
         "raceA", "raceB", "raceI", "raceP", "raceW", "cent_unemp","cent_crime", "cent_medinc"  
         
         # "PRSNTOTAL", "PRSNRATE",  "MURDER",                 
         #"MANSLAUGHTER", "FORCIBLE_SEX", "ASSAULT", "NON_FORCIBLE_SEX",       
         #"KIDNAPPING_ABDUCTION",    "HUMAN_TRAFFICKING",       "VIOL_OF_NO_CONTACT",      "PRPRTYTOTAL",            
         #"PRPRTYRATE",              "ARSON" ,                  "BRIBERY" ,                "BURGLARY"  ,             
         #"COUNTERFEITING_FORGERY",  "DESTRUCTION_OF_PROPERTY", "EXTORTION_BLACKMAIL",     "ROBBERY",                
         #"THEFT",                   "SCTYTOTAL" ,              "SCTYRATE" ,               "DRUG_VIOLATIONS" ,       
         #"GAMBLING_VIOLATIONS",     "PORNOGRAPHY" ,            "PROSTITUTION",            "WEAPON_LAW_VIOLATION" ,  
         #"ANIMAL_CRUELTY")
)

citation("xgboost")




#first we use catools to split the data into train and test
sample_split <- caTools::sample.split(Y = newdatk$left_dataset, SplitRatio = 0.7)
train_set <- subset(x = newdatk, sample_split == TRUE)
test_set <- subset(x = newdatk, sample_split == FALSE)

y_train <- as.integer(train_set$left_dataset) 
y_test <- as.integer(test_set$left_dataset) 
x_train <- train_set[,var]
x_test <- test_set[,var]

colnames(x_train) <- c("sex", "insurance_benefits", "salary", 
                       "years_experience", "hispanic", "county_median_house_price",
                       "county_percent_of_white_pop", "race_asian", "race_black", "race_indigenous",
                       "race_pacific", "race_white", "county_unemployment",
                       "county_crime_rate", "county_median_income"
                        )

colnames(x_test) <- c("sex", "insurance_benefits", "salary", 
                      "years_experience", "hispanic", "county_median_house_price",
                      "county_percent_of_white_pop", "race_asian", "race_black", "race_indigenous",
                      "race_pacific", "race_white", "county_unemployment",
                      "county_crime_rate", "county_median_income"
)

#we need to dummy everything for xgboost to work
#sparse_matrix_train <- Matrix::sparse.model.matrix(y_train ~ ., data = traincomp)[,-1]
#X_train_dmat = xgb.DMatrix(sparse_matrix_train, label = y_train)




x_train <- fastDummies::dummy_cols(x_train)
x_train <- x_train %>% select(-sex)

x_test <- fastDummies::dummy_cols(x_test)
x_test <- x_test %>% select(-sex)

#table(y_train)
#str(x_train)

#we have to use DMatrix because this is how XGBoost store data. It is a specific data structure
#used to store data in a way optimized for memory efficiency and training speed.


xgb_train <- xgboost::xgb.DMatrix(data = as.matrix(x_train), label = y_train)
xgb_test <- xgboost::xgb.DMatrix(data = as.matrix(x_test), label = y_test)

#we will go with the most basic parameter values
xgb_params <- list(
  booster = "gbtree",
  eta = 0.03,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = 2
)



#RUN THE MODEL
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100, #let's start with this and hope that we land on something
  verbose = 1
)

#xgb_model

importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = xgb_model
)
#importance_matrix
#try
#xgbImp1 <- xgb.importance(feature_names = colnames(xgb_train), model = xgb_model)
#xgbImp1 <- xgbImp1 %>% mutate(rank = dense_rank((Gain)))

#ggplot(data=xgbImp1[which(xgbImp1$rank <= 10),], aes(y = reorder(Feature, Gain), x = Gain)) +
 # geom_bar(stat="identity") + 
 # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 # labs(title = paste("Variable Importance (", group, " Teachers)", sep = ""), y = "Variable", x = "Information Gain
 #                                                                          ")
#plot the importance matrix
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, top_n = 10, xlab = "Information Improvement", 
                    measure = "Gain", main = paste("Variable Importance (", group, " Teachers)", sep = ""))

#xgb.plot.importance(importance_matrix) 
}
?paste

#####another
ml2 <- function() {
  ##start ML
  var <- c("sex", "cins", "tfinsal_centered", "quadexp", "exp", "hispanic", "medhouse", "perc_white",
           "raceA", "raceB", "raceI", "raceP", "raceW", "avqwint", 
           #"avqspring", "avqsummer", "avqfall", 
           "RATE", "medinc"  # "PRSNTOTAL", "PRSNRATE",  "MURDER",                 
           #"MANSLAUGHTER", "FORCIBLE_SEX", "ASSAULT", "NON_FORCIBLE_SEX",       
           #"KIDNAPPING_ABDUCTION",    "HUMAN_TRAFFICKING",       "VIOL_OF_NO_CONTACT",      "PRPRTYTOTAL",            
           #"PRPRTYRATE",              "ARSON" ,                  "BRIBERY" ,                "BURGLARY"  ,             
           #"COUNTERFEITING_FORGERY",  "DESTRUCTION_OF_PROPERTY", "EXTORTION_BLACKMAIL",     "ROBBERY",                
           #"THEFT",                   "SCTYTOTAL" ,              "SCTYRATE" ,               "DRUG_VIOLATIONS" ,       
           #"GAMBLING_VIOLATIONS",     "PORNOGRAPHY" ,            "PROSTITUTION",            "WEAPON_LAW_VIOLATION" ,  
           #"ANIMAL_CRUELTY")
  )
  
  
  
  
  
  #first we use catools to split the data into train and test
  sample_split <- caTools::sample.split(Y = newdatk$left_dataset, SplitRatio = 0.7)
  train_set <- subset(x = newdatk, sample_split == TRUE)
  test_set <- subset(x = newdatk, sample_split == FALSE)
  
  y_train <- as.integer(train_set$left_dataset) 
  y_test <- as.integer(test_set$left_dataset) 
  x_train <- train_set[,var]
  x_test <- test_set[,var]
  
  colnames(x_train) <- c("sex", "insurance", "district_centered_salary", 
                         "years_experience", "exp", "hispanic", "county_median_house_price",
                         "county_percent_of_white_pop", "race_asian", "race_black", "race_indigenous",
                         "race_pacific", "race_white", "winter_unemployment",
                         #"spring_unemployment", 
                         #"summer_unemployment", "fall_unemployment", 
                         "county_crime_rate", "county_median_income"
  )
  
  colnames(x_test) <- c("sex", "insurance", "district_centered_salary", 
                        "years_experience", "exp", "hispanic", "county_median_house_price",
                        "county_percent_of_white_pop", "race_asian", "race_black", "race_indigenous",
                        "race_pacific", "race_white", "winter_unemployment",
                        #"spring_unemployment", "summer_unemployment", "fall_unemployment", 
                        "county_crime_rate", "county_median_income"
  )
  
  #we need to dummy everything for xgboost to work
  #sparse_matrix_train <- Matrix::sparse.model.matrix(y_train ~ ., data = traincomp)[,-1]
  #X_train_dmat = xgb.DMatrix(sparse_matrix_train, label = y_train)
  
  
  
  
  x_train <- fastDummies::dummy_cols(x_train)
  x_train <- x_train %>% select(-sex)
  
  x_test <- fastDummies::dummy_cols(x_test)
  x_test <- x_test %>% select(-sex)
  
  #table(y_train)
  #str(x_train)
  
  #we have to use DMatrix because this is how XGBoost store data. It is a specific data structure
  #used to store data in a way optimized for memory efficiency and training speed.
  
  
  xgb_train <- xgboost::xgb.DMatrix(data = as.matrix(x_train), label = y_train)
  xgb_test <- xgboost::xgb.DMatrix(data = as.matrix(x_test), label = y_test)
  
  #we will go with the most basic parameter values
  xgb_params <- list(
    booster = "gbtree",
    eta = 0.03,
    max_depth = 8,
    gamma = 4,
    subsample = 0.75,
    colsample_bytree = 1,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = 2
  )
  
  
  
  #RUN THE MODEL
  xgb_model <- xgb.train(
    params = xgb_params,
    data = xgb_train,
    nrounds = 100, #let's start with this and hope that we land on something
    verbose = 1
  )
  
  #xgb_model
  
  importance_matrix <- xgb.importance(
    feature_names = colnames(xgb_train), 
    model = xgb_model
  )
  #importance_matrix
  
  #plot the importance matrix
  xgb.plot.importance(importance_matrix, measure = 'Cover') 
}

xgb.plot.importance(importance_matrix, measure = 'Frequency')
xgb.plot.importance(importance_matrix, measure = 'Cover')
xgb.plot.importance(importance_matrix)
?xgb.plot.importance


#predictions - predictors are returned in a form of probabilities
xgb_preds <- predict(xgb_model, as.matrix(x_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(as.factor(newdatk$left_dataset))





#see how well it predicts
xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- levels(as.factor(newdatk$left_dataset))[y_test + 1]
xgb_preds

#calculate over-all accuracy score

accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
accuracy #93% accuracy 

#confusion matrix
confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))

#plot confusion matrix
cm <- confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))
cfm <- as_tibble(cm$table)
cvms::plot_confusion_matrix(cfm, target_col = "Reference", prediction_col = "Prediction", counts_col = "n")



