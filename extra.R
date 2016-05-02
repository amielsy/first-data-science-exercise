# check accuracy of training by putting some of training data set to test data set
train1 <- combi[1:60000,]
test1 <- combi[60001:80104,]


library(rattle)
library(rpart.plot)
library(RColorBrewer)
set.seed(415)

fit <- randomForest(as.factor(readmitted) ~ race + gender + age + admission_type_id + discharge_disposition_id 
                    + admission_source_id + time_in_hospital + payer_code + num_lab_procedures
                    + num_medications + number_outpatient + number_emergency + number_inpatient + number_diagnoses
                    + max_glu_serum + metformin  + insulin +other_drugs
                     + change + diabetesMed + weight_known + A1Cresult 
                    ,  data=train1, importance=TRUE, ntree=30, mtry=4)

fit <- rpart(as.factor(readmitted) ~ race + gender + age + admission_type_id + discharge_disposition_id 
                    + admission_source_id + time_in_hospital + payer_code + num_lab_procedures
                    + num_medications + number_outpatient + number_emergency + number_inpatient + number_diagnoses
                    + max_glu_serum + metformin  + insulin +other_drugs
                    + change + diabetesMed + weight_known + A1Cresult 
                    ,  data=train1, method="class",control=rpart.control(minsplit=70, cp=0.001))

fancyRpartPlot(fit)

Prediction <- predict(fit, test1)
submit <- data.frame(readmitted = test1$readmitted, readmittedprediction = Prediction)
#write.csv(submit, file = "firstforest.csv", row.names = FALSE)
table(submit$readmitted,submit$readmittedprediction)

Prediction2 <- predict(fit, train1)
submit2 <- data.frame(readmitted = train1$readmitted, readmittedprediction = Prediction2)