library(ROSE)
library(randomForest)
library(caret)
library(e1071)
library(rpart)
library("rpart.plot")
library(naniar)
library(tidyverse)
library(ggplot2)
library("wesanderson")
library(ggridges)
library(ggpubr)
library(caret)
library(yardstick)
library("rattle")
library(rpart.plot)
library(RColorBrewer)
library(caTools)
# Importing the dataset
data<- read.csv("stroke.csv")
str(data)
table(data$stroke)
miss_scan_count(data = data, search = list("N/A","Unknown","Other")) # we don't have NA, or other,only Unknown
# New labeled data
data_nl <- data %>%
mutate(bmi = case_when(bmi < 18.5 ~ "Underweight",
                      bmi >= 18.5 & bmi < 25 ~ "Normal weight",
                      bmi >= 25 & bmi < 30 ~ "Overweight",
                      bmi >= 30 ~ "Obese"),
bmi = factor(bmi, levels = c("Underweight",
                            "Normal weight",
                            "Overweight",
                            "Obese"), order = TRUE))%>%
mutate(avg_glucose_level = case_when(avg_glucose_level < 100 ~ "Normal",
                                     avg_glucose_level >= 100 & avg_glucose_level <= 125 ~ "Prediabetes",
                                     avg_glucose_level > 125  ~ "Diabetes"),
avg_glucose_level = factor(avg_glucose_level, levels = c("Normal",
                                                        "Prediabetes",
                                                        "Diabetes"), order = TRUE)) %>%
mutate(age = case_when(age < 2 ~ "Baby",
                       age >= 2 & age <= 19 ~ "Teenager",
                       age > 19 & age < 30 ~ "Young adult",
                       age >= 30 & age < 55~ "Middle-age",
                       age >= 55 ~ "Senior"),
age = factor(age, levels = c("Baby",
                            "Teenager",
                            "Young adult",
                            "Middle-age",
                            "Senior"), order = TRUE))
data_nl$smoking_status <- factor(data_nl$smoking_status)
data_nl$ever_married <- factor(data_nl$ever_married)
data_nl$gender <- factor(data_nl$gender)
data_nl$heart_disease <- factor(data_nl$heart_disease,levels=c(0,1), labels=c("No","Yes"))
data_nl$hypertension <- factor(data_nl$hypertension, levels=c(0,1), labels=c("No","Yes"))
data_nl$work_type <- factor(data_nl$work_type)
data_nl$stroke <- factor(data_nl$stroke,
                         levels = c(0,1),
                         labels = c("Not Stroke","Stroke"))
data_nl$Residence_type <- factor(data_nl$Residence_type)
#typeof(data_nl$stroke)


##################### GRAPHS AND EXLANATORY ANALYSIS ##################################
table(data$stroke)
Stroke_plt <- data_nl %>%
  ggplot() +
  geom_bar(aes(x = stroke, fill = stroke), position = "dodge") +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "Count", title = "") + ylab("Number of cases") + xlab("")

Stroke_plt


Gender_plt1 <- data %>%
  ggplot() +
  geom_bar(aes(x = data_nl$gender, fill = gender), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[2], wes_palette("GrandBudapest2")[3])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Gender", title = "Gender Distribution")
Gender_plt1

Hypertension_plt1 <- data %>%
  ggplot() +
  geom_bar(aes(x = data_nl$hypertension, fill = data_nl$hypertension), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[2], wes_palette("GrandBudapest2")[3])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Hypertension", title = "Hypertension Distribution")

Heart_disease_plt1 <- data %>%
  ggplot() +
  geom_bar(aes(x = data_nl$heart_disease, fill = data_nl$heart_disease), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[2], wes_palette("GrandBudapest2")[3])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Heart Dsiease", title = "Heart Disease Distribution")

Ever_married_plt1 <- data %>%
  ggplot() +
  geom_bar(aes(x = data_nl$ever_married, fill = ever_married), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[2], wes_palette("GrandBudapest2")[3])) +
  theme_minimal() + 
  theme(plot.title = element_text( face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Ever Married", title = "Ever Married Distribution")

Work_type_plt1 <- data %>%
  ggplot() +
  geom_bar(aes(x = data_nl$work_type, fill = work_type), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[1], 
                               wes_palette("GrandBudapest2")[2],
                               wes_palette("GrandBudapest2")[3],
                               wes_palette("GrandBudapest2")[4],
                               wes_palette("GrandBudapest2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Work Type", title = "Work Type Distribution")

Residence_type_plt1 <- data %>%
  ggplot() +
  geom_bar(aes(x = data_nl$Residence_type, fill = Residence_type), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[2], wes_palette("GrandBudapest2")[3])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Residence Type", title = "Residence Type Distribution")
Smoking_status_plt1 <- data %>%
  ggplot() +
  geom_bar(aes(x = data_nl$smoking_status, fill = smoking_status), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[1], 
                               wes_palette("GrandBudapest2")[2],
                               wes_palette("GrandBudapest2")[3],
                               wes_palette("GrandBudapest2")[4])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Smoking Status", title = "Smoking Status Distribution")
ggarrange(Gender_plt1,
         Hypertension_plt1,
         Heart_disease_plt1,
         Ever_married_plt1,
         Work_type_plt1,
         Residence_type_plt1,
         Smoking_status_plt1,
         nrow  = 4,
         ncol = 2)



Hypertension_plt2 <- data_nl %>%
  ggplot() +
  geom_bar(aes(x = hypertension, fill = stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Hypertension", title = "Hypertension Distribution by Stroke Status")
Hypertension_plt2

Heart_disease_plt2 <- data_nl %>%
  ggplot() +
  geom_bar(aes(x = heart_disease, fill = stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Heart Disease", title = "Heart Disease Distribution by Stroke Status")
Heart_disease_plt2

Ever_married_plt2 <- data_nl %>%
  ggplot() +
  geom_bar(aes(x = ever_married, fill = stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Ever Married", title = "Ever Married Distribution by Stroke Status")
Ever_married_plt2
Work_type_plt2 <- data_nl %>%
  ggplot() +
  geom_bar(aes(x = work_type, fill = stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion", x = "Work Type", title = "Work Type Distribution by Stroke Status")
Work_type_plt2
Residence_type_plt2 <- data_nl %>%
  ggplot() +
  geom_bar(aes(x = Residence_type, fill = stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Residence Type", title = "Residence Type Distribution by Stroke Status")
Residence_type_plt2

Smoking_status_plt2 <- data_nl %>%
  ggplot() +
  geom_bar(aes(x = smoking_status, fill = stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Smoking Status", title = "Smoking Status Distribution by Stroke Status")

Smoking_status_plt2
Gender_plt2 <- data_nl %>%
  ggplot() +
  geom_bar(aes(x = gender, fill = stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion", x = "Gender", title = "Gender Distribution by Stroke Status")

ggarrange(Gender_plt2,
          Hypertension_plt2,
          Heart_disease_plt2,
          Ever_married_plt2,
          Work_type_plt2,
          Residence_type_plt2,
          Smoking_status_plt2,
          nrow  = 4,
          ncol = 2)

age_plot <- data %>% 
  ggplot() +
  geom_boxplot(aes(x = data_nl$stroke, y = age, fill = data_nl$stroke)) +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  labs(x = "Stroke", y = "Age", title = "")

age_plot
################ END OF GRAPHS ############################

############## DATA PREPROCESSING ##########################

data$smoking_status <- as.factor(data$smoking_status)
data$ever_married <- as.factor(data$ever_married)
data$gender <- as.factor(data$gender )
data$work_type<-as.factor(data$work_type)
data$Residence_type<-as.factor(data$Residence_type)
data$hypertension<-as.factor(data$hypertension)
data$heart_disease<-as.factor(data$heart_disease)

data$stroke <- factor(data$stroke,
                         levels = c(0,1),
                         labels = c("No.Stroke","Stroke"))
str(data)

############### Making TRAIN AND TEST SET ###################

set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(data$stroke, SplitRatio = 0.7)
training_data  <- subset(data, sample == TRUE) ##ovde svuda bilo final
testing_data   <- subset(data, sample == FALSE)

data_balanced <- ovun.sample(stroke~.,
                                   data = training_data,
                                   method = "both",
                                    seed = 2, p = 0.5)$data


table(data_balanced$stroke) # NOW IS BALANCED

model = rpart(stroke~., data = data_balanced, cp=0.02)
plotcp(model)
fancyRpartPlot(model,caption=NULL)
printcp(model)
set.seed(30)
cp_find <- train(
  stroke ~ .,
  data = data_balanced,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 50
)


ggplot(cp_find)+theme_minimal()
cp_find

ptree<- prune(model,cp= model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")
printcp(ptree)


# 
# library("plotmo")
# plotmo(ptree, type = "prob") 

# prediction--------------------------------------------------------------------
prediction = predict(model, newdata = data_balanced,type="class")
confusionMatrix(prediction,data_balanced$stroke,positive="Stroke")
prediction = predict(model, newdata = testing_data,type="class")
confusionMatrix(prediction,testing_data$stroke,positive="Stroke")

truth_predicted <- data.frame(
  obs =testing_data$stroke ,
  pred = prediction)

truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)

cm <- conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "gray90", high = "orange") 
prune$variable.importance

###### RANDOM FOREST

# rf_dropped<-randomForest(stroke ~ ., data = data_balanced, 
#                          ntree = 1000, mtry = 4, nodesize = 1, importance = TRUE, sampsize = 3000)
# 
# #create a confusion matrix
# rf_dropped.pred <- predict(rf_dropped, testing_data)
# confusionMatrix(rf_dropped.pred, testing_data$stroke,positive = "Stroke")
# 
# 
# table(data_balanced$stroke)
#######################################################


# model<-svm(stroke~.,data=data_balanced)
# summary(model)
# 
# #predicting values 
# pred_dropped<-predict(model,testing_data)
# 
# #Confusion Matrix 
# confusionMatrix(pred_dropped,testing_data$stroke,positive = "Stroke")


