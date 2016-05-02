#loadlibrary 
library(randomForest)
library(party)

train <- read.csv("~/R/sent_out/train.csv", quote="", na.strings="?")
test <- read.csv("~/R/sent_out/test.csv", quote="", na.strings="?")
test$readmitted <- NA

#Removed Expired, no point in machine learning since patient obvious does not readmit
train$discharge_disposition_id[train$discharge_disposition_id == 19] <- 11
train$discharge_disposition_id[train$discharge_disposition_id == 20] <- 11
train$discharge_disposition_id[train$discharge_disposition_id == 21] <- 11
train<-train[!(train$discharge_disposition_id==11),]

#train 80104 rows, test 20,353 rows
#to clean-up data for both test and train
combi <- rbind(train, test)

#change to factor instead of int
combi$admission_type_id <- as.factor(combi$admission_type_id)
combi$discharge_disposition_id <- as.factor(combi$discharge_disposition_id)
combi$admission_source_id <- as.factor(combi$admission_source_id)

#assume NA in race means other
combi$race <- as.character(combi$race)
combi$race[is.na(combi$race)] <- "Other"
combi$race <- factor(combi$race)

#change payer code and medical specialty NA to unknown for random forest to work
combi$payer_code <- as.character(combi$payer_code)
combi$payer_code[is.na(combi$payer_code)] <- "unknown"
combi$payer_code <- factor(combi$payer_code)

combi$medical_specialty <- as.character(combi$medical_specialty)
combi$medical_specialty[is.na(combi$medical_specialty)] <- "unknown"
combi$medical_specialty <- factor(combi$medical_specialty)

#create column weightknown, then drop weight column
combi$weight <- as.character(combi$weight)
combi$weight_known[is.na(combi$weight)] <- "unknown"
combi$weight_known[!is.na(combi$weight)] <- "known"
combi$weight_known <- factor(combi$weight_known)
combi <- subset( combi, select = -c(weight) )

#Drop Diagnosis since too many classifications

combi <- subset( combi, select = -c(diag_3,diag_2,diag_1) )

#Combine all drugs into one column except insulin and metformin, ignore medicine combinations
combi$other_drugs<-"No"

combi$other_drugs[combi$repaglinide == "Steady"|combi$nateglinide == "Steady"|combi$chlorpropamide == "Steady"|
                    combi$glimepiride == "Steady"|combi$acetohexamide == "Steady"|combi$glipizide == "Steady"|
                    combi$glyburide == "Steady"|combi$pioglitazone == "Steady"|combi$rosiglitazone == "Steady"|
                    combi$acarbose == "Steady"|combi$miglitol == "Steady"|combi$tolazamide == "Steady"|
                    combi$repaglinide == "Steady"|combi$repaglinide == "Steady"|combi$repaglinide == "Steady"
                  |combi$tolbutamide == "Steady"|combi$tolazamide == "Steady"]<-"Steady"

combi$other_drugs[combi$repaglinide == "Down"|combi$nateglinide == "Down"|combi$chlorpropamide == "Down"|
                    combi$glimepiride == "Down"|combi$acetohexamide == "Down"|combi$glipizide == "Down"|
                    combi$glyburide == "Down"|combi$pioglitazone == "Down"|combi$rosiglitazone == "Down"|
                    combi$acarbose == "Down"|combi$miglitol == "Down"|combi$tolazamide == "Down"|
                    combi$repaglinide == "Down"|combi$repaglinide == "Down"|combi$repaglinide == "Down"]<-"Down"

combi$other_drugs[combi$repaglinide == "Up"|combi$nateglinide == "Up"|combi$chlorpropamide == "Up"|
           combi$glimepiride == "Up"|combi$acetohexamide == "Up"|combi$glipizide == "Up"|
           combi$glyburide == "Up"|combi$pioglitazone == "Up"|combi$rosiglitazone == "Up"|
           combi$acarbose == "Up"|combi$miglitol == "Up"|combi$tolazamide == "Up"|
           combi$repaglinide == "Up"|combi$repaglinide == "Up"|combi$repaglinide == "Up"]<-"Up"

combi$other_drugs <- factor(combi$other_drugs)
#drop all medications except metformin and insulin
combi <- subset( combi, select = -c(chlorpropamide,acetohexamide,tolbutamide,
                                    acarbose,miglitol,troglitazone,tolazamide,examide,
                                    citoglipton,glipizide.metformin,glimepiride.pioglitazone,
                                    metformin.rosiglitazone, metformin.pioglitazone,repaglinide
                                    ,nateglinide,glimepiride,glipizide,glyburide,tolbutamide,
                                    rosiglitazone,acarbose) )

#Group together Not Available/NA/Unknown/NULL
combi$admission_type_id[combi$admission_type_id == 6] <- 5
combi$admission_type_id[combi$admission_type_id == 8] <- 5
combi$discharge_disposition_id[combi$discharge_disposition_id == 25] <- 18
combi$discharge_disposition_id[combi$discharge_disposition_id == 26] <- 18
combi$admission_source_id[combi$admission_source_id == 15] <- 9
combi$admission_source_id[combi$admission_source_id == 17] <- 9
combi$admission_source_id[combi$admission_source_id == 20] <- 9
combi$admission_source_id[combi$admission_source_id == 21] <- 9

#Group together Expired, Later can override prediction to no readmittance
combi$discharge_disposition_id[combi$discharge_disposition_id == 19] <- 11
combi$discharge_disposition_id[combi$discharge_disposition_id == 20] <- 11
combi$discharge_disposition_id[combi$discharge_disposition_id == 21] <- 11

#Separate train and test
train <- combi[1:80104,]
test <- combi[80105:100457,]

#modelling approach
set.seed(415)
fit <- randomForest(as.factor(readmitted) ~ race + gender + age + admission_type_id + discharge_disposition_id 
                    + admission_source_id + time_in_hospital + payer_code + num_lab_procedures
                    + num_medications + number_outpatient + number_emergency + number_inpatient + number_diagnoses
                    + max_glu_serum + metformin  + insulin +other_drugs
                    + change + diabetesMed + weight_known + A1Cresult 
                    ,  data=train, importance=TRUE, ntree=30, mtry=4)
Prediction <- predict(fit, test)
#Expired=No readmittance
Prediction[test$discharge_disposition_id == 11] <- "NO"

#save into csv
submit <- data.frame(readmitted = Prediction)
write.csv(submit, file = "MSD_FT_Amiel.csv", row.names = FALSE)
