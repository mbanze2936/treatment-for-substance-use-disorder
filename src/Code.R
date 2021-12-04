library('dplyr')
library('tidyverse')
library('ggplot2') 
library('cowplot')
library('caTools')
library('ElemStatLearn')
library('class')
library('e1071')
library('rpart')
library('rpart.plot')
library('randomForest')
library('caret')
library('knitr')
library('PerformanceAnalytics')
library('ROSE')
library('metan')
library('gmodels')
library('CGPfunctions')
library('lsr')
library('ggpubr')
library('pROC')
library('plotly')
library('reshape2')


# Data pre-processing

Dataset <- read.csv('data/Data.csv', header = TRUE, stringsAsFactors = FALSE)
str(Dataset)
summary(Dataset)
fix(Dataset)

patientData <- Dataset %>% select(CASEID, AGE, ALCDRUG, DSMCRIT, EDUC, EMPLOY, ETHNIC, FREQ1, FREQ2, FREQ3, FRSTUSE1,
                                  FRSTUSE2, FRSTUSE3, GENDER, LIVARAG, LOS, MARSTAT, METHUSE, NOPRIOR,
                                  PSOURCE, RACE, ROUTE1, ROUTE2, ROUTE3, SERVICES, SUB1, SUB2, SUB3, 
                                  VET, REASON)

#patientData %>% slice(1:1000)

# patientData_2 <- Dataset %>% select(AGE, ALCDRUG, DSMCRIT, EDUC, EMPLOY, ETHNIC, FREQ1, FREQ2, FREQ3, FRSTUSE1,
#                                   FRSTUSE2, FRSTUSE3, GENDER, LIVARAG, LOS, MARSTAT, METHUSE, NOPRIOR, PREG
#                                   PSOURCE, RACE, ROUTE1, ROUTE2, ROUTE3, SERVICES, SUB1, SUB2, SUB3,
#                                   VET, REASON)

#REASON - Target variable

str(patientData)
summary(patientData)

#Consider patients with REASON as Treatment Completed. Replace everything else with 0
patientData$REASON <- replace(patientData$REASON, patientData$REASON > 1, 0)



#Set the dependent variable - REASON as factor 
patientData$REASON <- as.factor(patientData$REASON)


#Remove rows with null values encoded as -9
patientData <- patientData[apply(patientData!= -9, 1, all),]
str(patientData)
dim(patientData)
head(patientData)


#Feature Selection using Boruta Algorithm
library(Boruta)
set.seed(111)
boruta <- Boruta(REASON~. , data = patientData, doTrace = 2, maxRuns = 20)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.6)

#Using Random Forest feature importance
rf_classifier <- randomForest(REASON ~. , data = train_set)

var_importance <- tibble(variable=setdiff(colnames(patientData), "REASON"),
                             importance=as.vector(importance(rf_classifier)))

var_importance <- arrange(var_importance, desc(importance))
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
p <- p + geom_bar(alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Variable Importance from Random Forest Algorithm")
p <- p + xlab("Patient Attributes") + ylab("Variable Importance (Mean Decrease in Gini Index)")
p <- p + scale_fill_discrete(name="Patient Attributes Name")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18),
          legend.title=element_text(size=16),
          legend.text=element_text(size=12))


# Bar chart to test each variable against the Target variable (REASON)
#Target vs Age
ggplot(data = patientData, 
       aes(AGE, fill = REASON)) +
         geom_bar(position = "dodge", 
                  alpha = 0.5) + 
         theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
                              labs(title = "Age vs Treatment Completed",
                                   x = "Age Group", y= "Treatment Status")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("12-14", "15-17","18-20","21-24","25-29","30-34","35-39","40-44",
                            "45-49","50-54","55-64","65 >"))+
  scale_fill_discrete(name="Treatment\nStatus")
                    

#Target vs Alcohol and other drugs
ggplot(data = patientData, 
       aes(ALCDRUG, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Alcohol and other drugs vs Treatment Completed",
       x = "Alcohol and other drugs", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs DSM diagnosis - Type of SUD


ggplot(data = patientData, 
       aes(DSMCRIT, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("12-14", "15-17","18-20","21-24","25-29","30-34","35-39","40-44",
                            "45-49","50-54","55-64","65 >"))
  labs(title = "DSM diagnosis vs Treatment Completed",
       x = "DSM diagnosis", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs Education
ggplot(data = patientData, 
       aes(EDUC, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels=c("No school/\n kg – grade 8","Grades 9 – 11","Grade 12/(GED)", 
                            "1-3 years of\nuniversity/college", "years of college\n/BA/BS/university"))+
  labs(title = "Education vs Treatment Completed",
       x = "Education", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs Employ
ggplot(data = patientData, 
       aes(EMPLOY, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Employment vs Treatment Completed",
       x = "Employment", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                       breaks = c("0","1"),
                       labels = c("Treatment not completed", "Treatment completed"))

#Target vs Ethnicity
ggplot(data = patientData, 
       aes(ETHNIC, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Ethnicity vs Treatment Completed",
       x = "Ethnicity", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs FREQ1
freq1p <- ggplot(data = patientData, 
       aes(FREQ1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Frequency of use at admission (primary) vs \nTreatment Completed",
       x = "Frequency", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs FREQ2
freq2p <-ggplot(data = patientData, 
       aes(FREQ2, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Frequency of use at admission (secondary) vs \nTreatment Completed",
       x = "Frequency", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs FREQ3
freq3p <-ggplot(data = patientData, 
       aes(FREQ3, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Frequency of use at admission (tertiary) vs \nTreatment Completed",
       x = "Frequency", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

freqp <- ggarrange(freq1p,freq2p,freq3p, ncol = 2, nrow = 2)
freqp


#Target vs FRSTUSE1
frst1 <-ggplot(data = patientData, 
                aes(FRSTUSE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Age at first use (primary) vs \nTreatment Completed",
       x = "Age at first use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs FRSTUSE2
frst2 <-ggplot(data = patientData, 
               aes(FRSTUSE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Age at first use (secondary) vs \nTreatment Completed",
       x = "Age at first use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs FRSTUSE3
frst3 <-ggplot(data = patientData, 
               aes(FRSTUSE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Age at first use (tertiary) vs \nTreatment Completed",
       x = "Age at first use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

frst <- ggarrange(frst1, frst2, frst3, ncol = 2, nrow = 2)
frst


#Target vs GENDER
ggplot(data = patientData, 
               aes(GENDER, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Gender vs Treatment Completed",
       x = "Gender", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs LIVARAG
ggplot(data = patientData, 
       aes(LIVARAG, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Living arrangements vs Treatment Completed",
       x = "Living arrangements", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs LOS
ggplot(data = patientData, 
       aes(LOS, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Length of stay at the facility vs \n Treatment Completed",
       x = "Length of stay", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs MARSTAT
ggplot(data = patientData, 
       aes(MARSTAT, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Marital status vs Treatment Completed",
       x = "Marital status", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs METHUSE
ggplot(data = patientData, 
       aes(METHUSE, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Medication-assisted opioid therapy vs \nTreatment Completed",
       x = "Medication-assisted opioid therapy", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs NOPRIOR
ggplot(data = patientData, 
       aes(NOPRIOR, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Previous substance use treatment episodes vs \nTreatment Completed",
       x = "Previous substance use treatment episodes", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs PSOURCE
ggplot(data = patientData, 
       aes(PSOURCE, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Referral source vs Treatment Completed",
       x = "Referral source", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs RACE
ggplot(data = patientData, 
       aes(RACE, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Race vs Treatment Completed",
       x = "Race", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs ROUTE1
rout1<- ggplot(data = patientData, 
       aes(ROUTE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Route of administration (primary) vs \nTreatment Completed",
       x = "Route of administration", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs ROUTE2
rout2<- ggplot(data = patientData, 
               aes(ROUTE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Route of administration (secondary) vs\n Treatment Completed",
       x = "Route of administration", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs ROUTE3
rout3<- ggplot(data = patientData, 
               aes(ROUTE3, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Route of administration (tertiary) vs \nTreatment Completed",
       x = "Route of administration", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

rout <- ggarrange(rout1,rout2,rout3, ncol = 2, nrow = 2)
rout

#Target vs Services
ggplot(data = patientData, 
       aes(SERVICES, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8'),
                   labels = c("Detox, 24-hour\nhospital inpatient","Detox, 24-hour", "Rehab/residential\n(non-detox)",
                              "Rehab/residential,\n(< 30 days)", "Rehab/residential\n(> 30 days)", "intensive\noutpatient",
                              "non-intensive\noutpatient","detoxification"))+
  labs(title = "Type of treatment/service setting at admission\n vs Treatment Completed",
       x = "Type of treatment/service", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))



#Target vs SUB1
subp1 <- patientData %>% filter(SUB1 != 1 & SUB1 != 6 & SUB1 != 8 & SUB1 != 9 & SUB1 <= 10) %>% ggplot(
               aes(x = SUB1, y = as.factor(REASON), fill = as.factor(REASON))) +
  geom_bar(stat = 'identity', 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/\nCrack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  labs(title = "Substance use at admission (primary) vs \nTreatment Completed",
       x = "Substance use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed")) 


#Target vs SUB2
subp2 <- patientData %>% filter(SUB2 != 1 & SUB2 != 6 & SUB2 != 8 & SUB2 != 9 & SUB2 <= 10) %>% ggplot(
  aes(x = SUB2, y = as.factor(REASON), fill = as.factor(REASON))) +
  geom_bar(stat = 'identity', 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/\nCrack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  labs(title = "Substance use at admission (secondary) vs \nTreatment Completed",
       x = "Substance use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c(" Treatment not completed", "Treatment completed"))

#Target vs SUB3
subp3 <- patientData %>% filter(SUB3 != 1 & SUB3 != 6 & SUB3 != 8 & SUB3 != 9 & SUB3 <= 10) %>%
  ggplot( aes(x = SUB3, y = as.factor(REASON), fill = as.factor(REASON))) +
  geom_bar(stat = 'identity', 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/\nCrack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  labs(title = "Substance use at admission (tertiary) vs \nTreatment Completed",
       x = "Substance use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

subp <- ggarrange(subp1,subp2, subp3, ncol = 2, nrow = 2)
subp

#Target vs VET
ggplot(data = patientData, 
       aes(VET, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Veteran Status vs Treatment Completed",
       x = "Veteran Status", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))



#Splitting data into train and test set
set.seed(189028) #1745

split <- sample.split(patientData$REASON, SplitRatio = 0.8)
train_set <- subset(patientData, split == TRUE)
test_set <- subset(patientData, split == FALSE)
prop.table(table(train_set$REASON))
summary(train_set)

dim(train_set)
dim(test_set)

#Feature Scaling
train_set[,1:29] <- scale(train_set[,1:29])
test_set[,1:29] <- scale(test_set[,1:29])

#Check for imbalanced dataset
barplot(prop.table(table(patientData$REASON)), 
        col = rainbow(2),
        ylim = c(0, 0.7), 
        main = "Class Distribution")
#The dataset is imbalanced

#Oversampling
table(train_set$REASON)
over <- ovun.sample(REASON ~., data = train_set, method = "over", N = 225440)$data
table(over$REASON)

barplot(prop.table(table(over$REASON)), 
        col = rainbow(2),
        ylim = c(0, 0.7), 
        main = "Class Distribution after Oversampling")

#Underfitting
table(train_set$REASON)
under <- ovun.sample(REASON ~., data = train_set, method = "under", N = 83788)$data
table(under$REASON)

#SMOTE
#Both
both <- ovun.sample(REASON~., data = train_set, method = "both", p=0.5, seed = 222, N= 154614)$data

table(both$REASON)

barplot(prop.table(table(both$REASON)), 
        col = rainbow(2),
        ylim = c(0, 0.7), 
        main = "Class Distribution after SMOTE")


#Logistic Regression
#Original dataset
logistic_classifier0 <- glm(formula = REASON ~ ., family = binomial, data = train_set)
prob_predict0 <- predict(logistic_classifier0, type = 'response', newdata = test_set[-30])
prob_predict1 <- predict(logistic_classifier0, type = 'response', newdata = train_set[-30])
y_pred0 <- ifelse(prob_predict1 > 0.5, 1, 0)
summary(logistic_classifier0)

#Confusion Matrix for Logistic Regression
print(cm <- table(test_set[, 30], y_pred0))
print(cm <- table(train_set[, 30], y_pred0))
print("Logistic Regression")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm)) * 100, "%")) #18503+8207/38653
print(paste("Error rate of the test set: ", (1-sum(diag(cm))/sum(cm)) * 100, "%")) #2266+9677
sensitivity(cm)
specificity(cm)

#Logistic Regression using Over fitting
logistic_classifier <- glm(formula = REASON ~ ., family = binomial, data = over)
prob_predict <- predict(logistic_classifier, type = 'response', newdata = test_set[-30])
prob_temp <- predict(logistic_classifier, type = 'link', newdata = test_set[-30])
y_pred <- ifelse(prob_predict > 0.5, 1, 0)
summary(logistic_classifier)

#Confusion Matrix for Logistic Regression
print(cm <- table(test_set[, 30], y_pred))
print("Logistic Regression")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm)) * 100, "%")) #18503+8207/38653
print(paste("Error rate of the test set: ", (1-sum(diag(cm))/sum(cm)) * 100, "%")) #2266+9677

#SMOTE
lr_both  <- glm(formula = REASON ~ ., family = binomial, data = both)
prob_predict2 <- predict(lr_both, type = 'response', newdata = test_set[-30])
prob_predict2 <- predict(lr_both, type = 'response', newdata = train_set[-30])
y_pred2 <- ifelse(prob_predict2 > 0.5, 1, 0)
print(cm <- table(test_set[, 30], y_pred2))
print(cm <- table(train_set[, 30], y_pred2))
print("Logistic Regression")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(cm))/sum(cm)) * 100, "%")) 

sensitivity(cm)
specificity(cm)


#CM plot
library(cvms)
library(tibble)
cfm <- as_tibble(cm, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "...1", 
                      prediction_col = "y_pred2",
                      counts_col = "n")

#ROC Curve
library(pROC)
roc <- plot(roc(test_set$REASON, prob_predict2, direction="<"),
     col="red", lwd=3)
legend(0.4, 0.3, round(auc(roc),3), title = "   AUC    ")



#K-Nearest Neighbors (KNN)
#Original dataset
knn_classifier0 <- knn(train = train_set[, -30], test = test_set[, -30], cl = train_set[, 30], k = 393, prob = TRUE) #k = 5 = 57%
#Confusion Matrix for KNN
print(knn_cm <- table(test_set[, 30], knn_classifier0))
print(knn_cm <- table(train_set[, 30], knn_classifier0))
print("K Nearest Neighbor")
print(paste("Accuracy of the test set: ", (sum(diag(knn_cm))/sum(knn_cm)) * 100, "%")) #16259+6373
print(paste("Error rate of the test set: ", (1-sum(diag(knn_cm))/sum(knn_cm)) *100, "%"))

confusionMatrix(table(test_set[, 30], knn_classifier0))

#Overfitting
knn_classifier <- knn(train = over[, -30], test = test_set[, -30], cl = over[, 30], k = 10, prob = TRUE) #k = 5 = 57%
#Confusion Matrix for KNN
print(knn_cm <- table(test_set[, 30], knn_classifier))
print("K Nearest Neighbor")
print(paste("Accuracy of the test set: ", (sum(diag(knn_cm))/sum(knn_cm)) * 100, "%")) #16259+6373
print(paste("Error rate of the test set: ", (1-sum(diag(knn_cm))/sum(knn_cm)) *100, "%"))


#SMOTE
knn_both <- knn(train = both[, -30], test = test_set[, -30], cl = both[, 30], k = 393, prob = TRUE) #k = 393, 440
#Confusion Matrix for KNN
print(knn_cm <- table(test_set[, 30], knn_both))
print("K Nearest Neighbor")
print(paste("Accuracy of the test set: ", (sum(diag(knn_cm))/sum(knn_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(knn_cm))/sum(knn_cm)) *100, "%"))

confusionMatrix(table(test_set[, 30], knn_both))

#CM plot
cfm <- as_tibble(knn_cm, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "...1", 
                      prediction_col = "knn_both",
                      counts_col = "n")

#ROC Curve

roc <- plot(roc(test_set$REASON, as.numeric(knn_both), direction="<"),
            col="green", lwd=3)
legend(0.4, 0.3, round(roc$auc,3), title = "        AUC     ")





#Naive Bayes
#Original dataset
naive_classifier0 <- naiveBayes(x = train_set[-30], y = train_set$REASON)
summary(naive_classifier0)
naive_pred0 <- predict(naive_classifier0, newdata = test_set[-30])
naive_pred1 <- predict(naive_classifier0, newdata = train_set[-30])
#Confusion Matrix for Navive Bayes
print(naive_cm <- table(test_set[, 30], naive_pred0))
print(naive_cm <- table(train_set[, 30], naive_pred1))
print("Naive Bayes")
print(paste("Accuracy of the test set: ",(sum(diag(naive_cm))/sum(naive_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(naive_cm))/sum(naive_cm)) * 100, "%"))
confusionMatrix(table(test_set[, 30], naive_pred0))
sensitivity(naive_cm)
specificity(naive_cm)

#Overfitting
naive_classifier <- naiveBayes(x = over[-30], y = over$REASON)
summary(naive_classifier)
naive_pred <- predict(naive_classifier, newdata = test_set[-30])
#Confusion Matrix for Navive Bayes
print(naive_cm <- table(test_set[, 30], naive_pred))
print("Naive Bayes")
print(paste("Accuracy of the test set: ",(sum(diag(naive_cm))/sum(naive_cm)) * 100, "%")) #20007+6191
print(paste("Error rate of the test set: ", (1-sum(diag(naive_cm))/sum(naive_cm)) * 100, "%"))

#SMOTE
naive_both <- naiveBayes(x = both[-30], y = both$REASON)
naive_pred1 <- predict(naive_both, newdata = test_set[-30])
naive_pred2 <- predict(naive_both, newdata = train_set[-30])
#Confusion Matrix for Navive Bayes
print(naive_cm <- table(test_set[, 30], naive_pred1))
print(naive_cm <- table(train_set[, 30], naive_pred2))
print("Naive Bayes")
print(paste("Accuracy of the test set: ",(sum(diag(naive_cm))/sum(naive_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(naive_cm))/sum(naive_cm)) * 100, "%"))
sensitivity(naive_cm)
specificity(naive_cm)
confusionMatrix(table(test_set[, 30], naive_pred1))

naive_both

#CM plot
cfm <- as_tibble(naive_cm, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "...1", 
                      prediction_col = "naive_pred1",
                      counts_col = "n")

#ROC
p <- predict(naive_both, test_set[,-30], type = 'raw')
roc <- roc(test_set$REASON, p[,2]) #Treatment completed
auc(roc)
plot(roc, col = c(6))
legend(0.4, 0.3, round(roc$auc,3), title = "        AUC     ")


#Decision Trees
#Original dataset
decision_tree_classifier0 = rpart(REASON~., data = train_set, method = "class")
rpart.plot(decision_tree_classifier0)
predictions0 = predict(decision_tree_classifier0, test_set[,-30], type="class")
predictions01 = predict(decision_tree_classifier0, train_set[,-30], type="class")
print("Decision Tree")
print(dt_cm <- table(predictions0, test_set$REASON))
print(dt_cm <- table(predictions01, train_set$REASON))
print(paste("Accuracy of the test set: ", (sum(diag(dt_cm))/sum(dt_cm)) * 100, "%")) #22444+6525
print(paste("Error rate of the test set: ", (1-sum(diag(dt_cm))/sum(dt_cm)) * 100, "%"))
sensitivity(dt_cm)
specificity(dt_cm)
confusionMatrix(table(test_set[, 30], predictions0))

#Overfitting
decision_tree_classifier = rpart(REASON~., data = over, method = "class")
rpart.plot(decision_tree_classifier)
predictions = predict(decision_tree_classifier, test_set[,-30], type="class")
print("Decision Tree")
print(dt_cm <- table(predictions, test_set$REASON))
print(paste("Accuracy of the test set: ", (sum(diag(dt_cm))/sum(dt_cm)) * 100, "%")) #22444+6525
print(paste("Error rate of the test set: ", (1-sum(diag(dt_cm))/sum(dt_cm)) * 100, "%"))

#SMOTE
decision_tree_both = rpart(REASON~., data = both, method = "class")
rpart.plot(decision_tree_both)
predictions1 = predict(decision_tree_both, test_set[,-30], type="class")
predictions12 = predict(decision_tree_both, train_set[,-30], type="class")
print("Decision Tree")
print(dt_cm <- table(predictions12, train_set$REASON))
print(paste("Accuracy of the test set: ", (sum(diag(dt_cm))/sum(dt_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(dt_cm))/sum(dt_cm)) * 100, "%"))

confusionMatrix(table(test_set[, 30], predictions1))
sensitivity(dt_cm)
specificity(dt_cm)
printcp(decision_tree_both)
plotcp(decision_tree_both)

#Pruning decision trees
dt_prune <- prune(decision_tree_both, cp = 0.014)
rpart.plot(dt_prune)
dt_pred = predict(dt_prune, test_set[,-30], type="class")
dt_pred = predict(dt_prune, train_set[,-30], type="class")
print("Pruned Decision Tree")
print(dt_prune <- table(dt_pred, train_set$REASON))
print(paste("Accuracy of the test set: ", (sum(diag(dt_prune))/sum(dt_prune)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(dt_prune))/sum(dt_prune)) * 100, "%"))
sensitivity(dt_prune)
specificity(dt_prune)
confusionMatrix(table(test_set[, 30], dt_pred))

#CM plot
cfm <- as_tibble(dt_prune, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "dt_pred", 
                      prediction_col = "...2",
                      counts_col = "n")

#ROC
p <- predict(decision_tree_both, test_set[,-30], type = 'prob')
roc <- roc(test_set$REASON, p[,2]) #Treatment completed
auc(roc)
plot(roc, col = "blue")
legend(0.4, 0.3, round(roc$auc,3), title = "        AUC     ")




#Random Forest 
#Original dataset
rf_classifier <- randomForest(REASON ~. , data = train_set, ntree = 10)
confusionMatrix(predict(rf_classifier, test_set), test_set$REASON, positive = '1')
rf_pred0 = predict(rf_classifier, test_set[,-30])
rf_cm <- table(test_set[, 30], rf_pred0)

confusionMatrix(predict(rf_classifier, train_set), train_set$REASON, positive = '1')
rf_pred0 = predict(rf_classifier, train_set[,-30])
rf_cm <- table(train_set[, 30], rf_pred0)
print(paste("Accuracy of the test set: ",(sum(diag(rf_cm))/sum(rf_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", 1-sum(diag(rf_cm))/sum(rf_cm), "%"))

print(rf_classifier)


#Overfitting
rf_over <- randomForest(REASON ~. , data = over, ntree = 10)
confusionMatrix(predict(rf_over, test_set), test_set$REASON, positive = '1')
rf_pred = predict(rf_over, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred))
print(paste("Accuracy of the test set: ",(sum(diag(rf_cm))/sum(rf_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", 1-sum(diag(rf_cm))/sum(rf_cm) * 100, "%"))

print(rf_over)
#Improved in the number of predicting 1 from 5503 to 5867

#Underfitting
rf_under <- randomForest(REASON ~. , data = under, ntree = 10)
confusionMatrix(predict(rf_under, test_set), test_set$REASON, positive = '1')
rf_pred2 = predict(rf_under, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred2))
print("Random Forest")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm)) * 100, "%"))
print(paste("Error rate of the test set: ",(1-sum(diag(cm))/sum(cm)) * 100, "%"))
print(rf_under)
#Loss of data and loss of accuracy 74.77, predicted 1 from 5503 to 8076

#SMOTE
#Both

rf_both <- randomForest(REASON ~. , data = both, ntree = 500)
confusionMatrix(predict(rf_both, test_set), test_set$REASON, positive = '1')
rf_pred_both = predict(rf_both, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred_both))

confusionMatrix(predict(rf_both, train_set), train_set$REASON, positive = '1')
rf_pred_both = predict(rf_both, train_set[,-30])
print(rf_cm <- table(train_set[, 30], rf_pred_both))
print("Random Forest")
print(paste("Accuracy of the test set: ",(sum(diag(rf_cm))/sum(rf_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(rf_cm))/sum(rf_cm)), "%"))
print(rf_both)
sensitivity(rf_cm)
specificity(rf_cm)


rf_pred_both = predict(rf, test_set[,-30], type = 'prob')
roc <- roc(test_set$REASON, rf_pred_both[,2]) #Treatment completed
auc(roc)
plot(roc, col = "dark blue")
legend(0.4, 0.3, round(roc$auc,4), title = "        AUC     ")

#CM plot
cfm <- as_tibble(rf_cm, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "...1", 
                      prediction_col = "rf_pred_both",
                      counts_col = "n")


model_tuned <- tuneRF(
  x=both[,-30], 
  y=both$REASON, 
  ntreeTry=500,
  mtryStart=4, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE 
) 

bestmodel <- model_tuned[model_tuned[, 2] == min(model_tuned[, 2]), 1]
print(model_tuned)
print(bestmodel)
rf <-randomForest(REASON~.,data=both, mtry=9, importance=TRUE,ntree=500) 
print(rf)
plot(Random_Forest_Model)


#Confusion Matrix after fine tuning
confusionMatrix(predict(rf, test_set), test_set$REASON, positive = '1')
rf_pred_both = predict(rf, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred_both))

confusionMatrix(predict(rf, train_set), train_set$REASON, positive = '1')
rf_pred_both = predict(rf, train_set[,-30])
print(rf_cm <- table(train_set[, 30], rf_pred_both))
print("Random Forest")
print(paste("Accuracy of the test set: ",round((sum(diag(rf_cm))/sum(rf_cm)),2) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(rf_cm))/sum(rf_cm))*100, "%"))
sensitivity(rf_cm)
specificity(rf_cm)



#Evaluate variable importance
Random_Forest_Model <- rf
importance(Random_Forest_Model)
varImpPlot(Random_Forest_Model)




# Test cases
new <- data.frame(AGE = 9, SUB1 = 10, LOS = 5, SERVICES = 1, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 2, SUB3 = 3, VET = 2)
res <- predict(rf, newdata=new)
print(ifelse(res == 1, 'Treatment Successful', 'Treatment Incomplete'))


new <- data.frame(AGE = 9, SUB1 = 10, LOS = 7 , SERVICES = 2, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

r <- predict(rf, newdata=new)
print(ifelse(res == 1, 'Treatment Successful', 'Treatment Incomplete'))




#Visualize the result
random_forest_model <- rf
varImpPlot(random_forest_model)
varImp(rf_both)
plot(random_forest_model)
print(rf_both)





#Length of stay vs other factors
#1 LOS vs Service and reason for discharge

Dataset %>%
  ggplot(aes(x = SERVICES, y = LOS, fill = as.factor(REASON))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Length of stay w.r.t reason of discharge\n and treament type",
       x = "Treatment type", y= "Length of stay (LOS) in days")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8"),
                   labels = c("Hospital\nInpatient", "Free-standing\nresidential",
                              "Hospital\nnon detox", "short-term\n< 30", "long-term\n> 30",
                              "Intensive\noutpatient", "Non-Intensive\noutpatient","Detoxification"))+
  scale_fill_discrete(name="Reason for discharge",
                      breaks = c("1","2","3","4","5","6","7"),
                      labels = c("Treatment Completed", "Dropped out", "Terminated by facility",
                                 "Transferred", "Incarcerated", "Death", "Other"))

#2 Pie chart to view type of treatments administered at max based on treatment completed
ser = c("Hospital\nInpatient", "Free-standing\nresidential",
           "Hospital\nnon detox", "short-term\n< 30", "long-term\n> 30",
           "Intensive\noutpatient", "Non-Intensive\noutpatient","Detoxification")
pie(table(Dataset$SERVICES), labels= ser, col=rainbow(length(ser)), main="Type of treatment service at discharge")

#3 LOS and alcohol dependence/ other SUD and completing treatment
#LOS VS SUB1

Dataset %>%
  filter((SUB1 == 2 | SUB1 == 3 |SUB1 == 4 |SUB1 == 5 |SUB1 == 7 |SUB1 == 10) & REASON == 1) %>%
  ggplot(aes(x = SUB1, y = LOS, fill = as.factor(SUB1))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Length of stay w.r.t alcohol and other substance use ",
       x = "Type of Substance used", y= "Length of stay (LOS) in days")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/Crack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  scale_fill_discrete(name="Type of SUD",
                      breaks = c("1","2","3","4","5","6","7",'8','9',"10"),
                      labels = c("","Alcohol", "Cocaine/Crack",
                                 "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                                 "Methamphetamine/speed"))


Dataset %>%
  filter(SUB1 == 2 | SUB1 == 3 |SUB1 == 4 |SUB1 == 5 |SUB1 == 7 |SUB1 == 10) %>%
  ggplot(aes(x = SUB1, y = REASON, fill = as.factor(REASON))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Substance use at primary ",
       x = "Type of Substance used", y= "Length of stay (LOS) in days")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/Crack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  scale_fill_discrete(name="Type of SUD",
                      breaks = c("1","2","3","4","5","6","7",'8','9',"10"),
                      labels = c("","Alcohol", "Cocaine/Crack",
                                 "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                                 "Methamphetamine/speed"))


#5 Race vs Daywait to enter facility 



#11 freq1 vs firstuse1
Dataset %>%
  filter(SUB1 == 2 | SUB1 == 3 |SUB1 == 4 |SUB1 == 5 |SUB1 == 7 |SUB1 == 10) %>%
  ggplot(aes(x = SUB1, y = AGE, fill = as.factor(GENDER))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Age at first substance abuse with gender",
       x = "Substance type", y = "Age")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/Crack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  # scale_y_continuous(limits = c("1","2","3","4","5","6","7",'8','9',"10", "11","12"),
  #                  labels = c("12-14", "15-17","18-20","21-24","25-29","30-34","35-39","40-44",
  #                             "45-49","50-54","55-64","65 >"))+
  scale_fill_discrete(name="Reason for discharge",
                      breaks = c("1","2"),
                      labels = c("Male","Female"))

#12 freq los
Dataset%>%
  filter((SUB1 == 2 | SUB1 == 3 |SUB1 == 4 |SUB1 == 5 |SUB1 == 7 |SUB1 == 10) & FREQ1 != -9) %>%
  ggplot( aes(x= as.factor(SUB1), y= LOS, fill=as.factor(FREQ1))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Frequency of Substance intake before entering the facility with time spent",
       x = "Substance type", y = "Length of stay at facility")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/Crack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  scale_fill_discrete(name="Prior usage",
                      breaks = c("1","2","3"),
                      labels = c("No use in the past month","Some use","Daily Use"))

##
Dataset %>% filter(ETHNIC != -9 & DAYWAIT != -9) %>%
ggplot( aes(x = DAYWAIT,
           fill = as.factor(ETHNIC))) +
  geom_density(alpha = 0.4) +
  labs(title = "Daywait for Race and ethnicity ")

##

