# **************** LIBRARIES ********************************
library(RCurl)
library(jsonlite)
library(tidyverse)
library(imputeTS)
library(ggplot2)
library(arulesViz)
library(arules)
library(kernlab)
library(dplyr)
library(maps)

# *******************************************
JSONfile <- "fall2019-survey-m08.json"
converted_data <- jsonlite::fromJSON(JSONfile)

# **************** CLEANING ********************************

cleaned_data <- converted_data

cleaned_data <- cleaned_data[,-which(colnames(cleaned_data)=="freeText")]

colnames(cleaned_data) <- colnames(cleaned_data) %>% str_replace_all("\\.","_") 

cleaned_data$Departure_Delay_in_Minutes[which(is.na(cleaned_data$Departure_Delay_in_Minutes))] = median(cleaned_data$Departure_Delay_in_Minutes,na.rm = TRUE)
cleaned_data$Arrival_Delay_in_Minutes[which(is.na(cleaned_data$Arrival_Delay_in_Minutes))] = median(cleaned_data$Arrival_Delay_in_Minutes,na.rm = TRUE)

cleaned_data <- cleaned_data %>% arrange(Flight_Distance)
cleaned_data$Flight_time_in_minutes <- na_interpolation(cleaned_data$Flight_time_in_minutes)

# **************** TRANSFORMATION ********************************

transform_data <- cleaned_data

summary(transform_data$Age)

for (var in 1:dim(transform_data)[1]) {
  if ((transform_data$Age[var]>=15) & (transform_data$Age[var]<33)) 
  { transform_data$t_age[var] <- 1 
  } else if ((transform_data$Age[var]>=33) & (transform_data$Age[var]<45)) 
  { transform_data$t_age[var] <- 2
  } else if ((transform_data$Age[var]>=45) & (transform_data$Age[var]<59)) 
  { transform_data$t_age[var] <- 3
  } else
    transform_data$t_age[var] <- 4 
}

summary(transform_data$Arrival_Delay_in_Minutes)

for (var in 1:dim(transform_data)[1]) {
  if(transform_data$Arrival_Delay_in_Minutes[var] > 5) {
    transform_data$Arrival_Delay_Greater_5[var] = 1 }
  else 
    transform_data$Arrival_Delay_Greater_5[var] = 0
}

summary(transform_data$Flight_time_in_minutes)

for (var in 1:dim(transform_data)[1]) {
  if(transform_data$Flight_time_in_minutes[var] > 92) {
    transform_data$Long_duration_flight[var] = TRUE }
  else
    transform_data$Long_duration_flight[var] = FALSE
}

summary(transform_data$Loyalty)

for (var in 1:dim(transform_data)[1]) {
  if((transform_data$Loyalty[var] >= -1) && (transform_data$Loyalty[var] < -0.5)) {
    transform_data$LoyalityIndex[var] = 1 }
  else if((transform_data$Loyalty[var] >= -0.5) && (transform_data$Loyalty[var] < 0)) {
    transform_data$LoyalityIndex[var] = 2 }
  else if((transform_data$Loyalty[var] >= 0) && (transform_data$Loyalty[var] < 0.5)) {
    transform_data$LoyalityIndex[var] = 3 }
  else 
    transform_data$LoyalityIndex[var] = 4
}

#transform_data <- transform_data %>% mutate(if((transform_data$Partner_Name == "Cool&Young Airlines Inc.") || (transform_data$Partner_Name == "FlyFast Airways Inc.") || (transform_data$Partner_Name == "FlyToSun Airlines Inc.") || (transform_data$Partner_Name == "FlyHere Airways") || (transform_data$Partner_Name == "Northwest Business Airlines Inc.")|| (transform_data$Partner_Name == "Paul Smith Airlines Inc.")|| (transform_data$Partner_Name == "Sigma Airlines Inc.")|| (transform_data$Partner_Name == "Southeast Airlines Co.")) {High_Impact_Group = 1 }
#                                else {High_Impact_Group = 0})
#dim(transform_data)
#colnames(transform_data)[37] <- "High_Impact_Group"



# **************** VISUALIZATION ********************************


visualize_data <- transform_data
# ----------------- Bar Plot 1-----------------
airlines <- visualize_data

lowersa<- airlines[which(airlines$Likelihood_to_recommend<7),]
highersa<- airlines[which(airlines$Likelihood_to_recommend>8),]

model<- lm(formula = airlines$Likelihood_to_recommend~airlines$Age+airlines$Gender, data = airlines)
summary(model)

#which(is.na(airlines))

#Bar Charts about Age & satisfaction
hist(airlines$Age)
highersa%>%
  ggplot()+
  aes(x=highersa$Age)+
  geom_histogram(binwidth = 2, color="black", fill="blue")
#Customers' age from 30 to 50 have the higher possibilities to be satisfied

lowersa%>%
  ggplot()+
  aes(x=lowersa$Age)+
  geom_histogram(binwidth = 1, color="black", fill="blue")
#Customers' age under 20 and over 80, and from 60 to 70 have higher possibilities of being unsatisfied.

#Flights attributes
summary(airlines$Departure_Delay_in_Minutes)
summary(airlines$Arrival_Delay_in_Minutes)

mean(airlines$Departure_Delay_in_Minutes)
#Categorize arrival delay
airlines <- airlines %>%
  mutate(Arrival_group = cut(Arrival_Delay_in_Minutes,
                             breaks = c(0, 100, 400, 730),
                             labels = c("low","medium","high")))
airlines$Arrival_group[is.na(airlines$Arrival_group)]<- c("low")
#Categorize departure delay
airlines <- airlines %>%
  mutate(Departure_group = cut(Departure_Delay_in_Minutes,
                               breaks = c(0, 100, 400, 730),
                               labels = c("low","medium","high")))
airlines$Departure_group[is.na(airlines$Departure_group)]<- c("low")
view(airlines)

#Barplot of short departure delay
departure_low<- airlines%>%
  filter(airlines$Departure_group=="low")%>%
  group_by(Partner_Name)
view(departure_low)
delay_low<- as.data.frame(table(departure_low$Partner_Name))
view(delay_low)
colnames(delay_low)[which(names(delay_low) == "Var1")] <- "AirlinesPartners"

ggplot(delay_low)+
  aes(x=reorder(AirlinesPartners,Freq),y=Freq)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Short Departure Delay")+
  labs(x="AirlinesPartners")+
  geom_col()

#Barplot of medium departure delay
departure_medium<- airlines%>%
  filter(airlines$Departure_group=="medium")%>%
  group_by(Partner_Name)
delay_medium<- as.data.frame(table(departure_medium$Partner_Name))
view(delay_medium)
colnames(delay_medium)[which(names(delay_medium) == "Var1")] <- "AirlinesPartners"
ggplot(delay_medium)+
  aes(x=reorder(AirlinesPartners,Freq),y=Freq)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Medium Departure Delay")+
  labs(x="AirlinesPartners")+
  geom_col()

#Barplot of long departure delay
departure_high<- airlines%>%
  filter(airlines$Departure_group=="high")%>%
  group_by(Partner_Name)
delay_high<- as.data.frame(table(departure_high$Partner_Name))
view(delay_high)
colnames(delay_high)[which(names(delay_high) == "Var1")] <- "AirlinesPartners"
ggplot(delay_high)+
  aes(x=AirlinesPartners,y=Freq)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Long Departure Delay")+
  geom_col()


#Barplot of short arrival delay
arrival_low<- airlines%>%
  filter(airlines$Arrival_group=="low")%>%
  group_by(Partner_Name)
view(arrival_low)
delay_low1<- as.data.frame(table(arrival_low$Partner_Name))
view(delay_low1)
colnames(delay_low1)[which(names(delay_low1) == "Var1")] <- "AirlinesPartners"
ggplot(delay_low1)+
  aes(x=reorder(AirlinesPartners,Freq),y=Freq)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="AirlinesPartners")+
  ggtitle("Short Arrival Delay")+
  geom_col()

#Barplot of medium arrival delay
arrival_medium<- airlines%>%
  filter(airlines$Arrival_group=="medium")%>%
  group_by(Partner_Name)
view(arrival_medium)
delay_medium1<- as.data.frame(table(arrival_medium$Partner_Name))
view(delay_medium1)
colnames(delay_medium1)[which(names(delay_medium1) == "Var1")] <- "AirlinesPartners"
ggplot(delay_medium1)+
  aes(x=reorder(AirlinesPartners,Freq),y=Freq)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Medium Arrival Delay")+
  labs(x="AirlinesPartners")+
  geom_col()

#Barplot of long arrival delay
arrival_high<- airlines%>%
  filter(airlines$Arrival_group=="high")%>%
  group_by(Partner_Name)
view(arrival_high)
delay_high1<- as.data.frame(table(arrival_high$Partner_Name))
view(delay_high1)
colnames(delay_high1)[which(names(delay_high1) == "Var1")] <- "AirlinesPartners"
ggplot(delay_high1)+
  aes(x=reorder(AirlinesPartners,Freq),y=Freq)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Long Arrival Delay")+
  labs(x="AirlinesPartners")+
  geom_col()


# ----------------- Bar Plot 2-----------------


jsonfile_data <- transform_data 
#view(jsonfile_data)


jsonfile_data <- jsonfile_data %>%
  mutate(Age_Category = cut(Age,
                            breaks = c(0, 10, 18, 59 ,100),
                            labels = c("Child","Teenager","Adult","Old")))

jsonfile_data <- jsonfile_data %>%
  mutate(Passenger_Category = cut(Likelihood_to_recommend,
                                  breaks = c(0, 6, 8, 10),
                                  labels = c("detractor","passive","promoter")))


jsonfile_data <- jsonfile_data %>%
  mutate(Passenger_Sensitivity = cut(Price_Sensitivity,
                                     breaks = c(0,3,5),
                                     labels = c("Low","High")))

summary(jsonfile_data$Arrival_Delay_in_Minutes)

jsonfile_data <- jsonfile_data %>%
  mutate(Arrival_group = cut(Arrival_Delay_in_Minutes,
                             breaks = c(-1,100,400,730),
                             labels = c("Low","Medium","High")))


jsonfile_data <- jsonfile_data %>%
  mutate(Delay_group = cut(Departure_Delay_in_Minutes,
                           breaks = c(-1,100,400,751),
                           labels = c("Low","Medium","High")))

###################AGE PLOT##################################

age_detractor <- jsonfile_data %>%
  filter(Passenger_Category == "detractor") %>%
  group_by(Age_Category) %>%
  summarise(detractor_freq_age = n())

View(age_detractor)


age_promoter <- jsonfile_data %>%
  filter(Passenger_Category == "promoter") %>%
  group_by(Age_Category) %>%
  summarise(promoter_freq_age = n())

#View(age_promoter)

age_total <- jsonfile_data %>%
  group_by(Age_Category) %>%
  summarise(total_freq_age = n())
view(age_total)

Age_merged= merge(age_detractor,age_promoter, by="Age_Category")

Age_merged= merge(Age_merged,age_total,by ="Age_Category")

View(Age_merged)

Age_merged$Age_detractor_percent = (Age_merged$detractor_freq_age/Age_merged$total_freq_age)*100

Age_merged$Age_promoter_percent = (Age_merged$promoter_freq_age/Age_merged$total_freq_age)*100

Age_merged$NPS = Age_merged$Age_promoter_percent - Age_merged$Age_detractor_percent


ggplot(Age_merged) +
  aes(x=Age_Category,y=NPS)+
  geom_col()+
  ggtitle("barplot for age")

###################Gender PLOT##################################


Gender_detractor <- jsonfile_data %>%
  filter(Passenger_Category == "detractor") %>%
  group_by(Gender) %>%
  summarise(detractor_freq_gender = n())

Gender_promoter <- jsonfile_data %>%
  filter(Passenger_Category == "promoter") %>%
  group_by(Gender) %>%
  summarise(promoter_freq_gender = n())

Gender_total <- jsonfile_data%>%
  group_by(Gender) %>%
  summarise(total_freq_gender = n())


Gender_merged = merge(Gender_detractor,Gender_promoter,by="Gender")
Gender_merged = merge(Gender_total,Gender_merged,by="Gender")

View(Gender_merged)

Gender_merged$Gender_detractor_percent=(Gender_merged$detractor_freq_gender/Gender_merged$total_freq_gender)*100

Gender_merged$Gender_promoter_percent=(Gender_merged$promoter_freq_gender/Gender_merged$total_freq_gender)*100


Gender_merged$Gender_NPS=Gender_merged$Gender_promoter_percent - Gender_merged$Gender_detractor_percent

ggplot(Gender_merged)+
  aes(x=Gender,y=Gender_NPS)+
  geom_col()+
  ggtitle("Plot for Gender")


###############class Plot#################

View(jsonfile_data)


Class_detractor <- jsonfile_data %>%
  filter(Passenger_Category == "detractor") %>%
  group_by(Class) %>%
  summarise(detractor_freq_class = n())

View(Class_detractor)

Class_promoter <- jsonfile_data %>%
  filter(Passenger_Category == "promoter") %>%
  group_by(Class) %>%
  summarise(promoter_freq_class = n())

Class_promoter

Class_total <- jsonfile_data %>%
  group_by(Class) %>%
  summarise(total_freq_class = n())

Class_total


Class_merged = merge(Class_promoter,Class_detractor,by="Class")
Class_merged = merge(Class_total,Class_merged,by="Class")

View(Class_merged)

Class_merged$detractor_class_percent = (Class_merged$detractor_freq_class/Class_merged$total_freq_class)*100

Class_merged$promoter_class_percent = (Class_merged$promoter_freq_class/Class_merged$total_freq_class)*100

Class_merged$class_NPS = Class_merged$promoter_class_percent - Class_merged$detractor_class_percent
#Class_merged


ggplot(Class_merged)+
  aes(x=Class,y=class_NPS)+
  geom_col()+
  ggtitle("Plot for class")


###############Partner Airlines Plot#################

Partner_Name_detractor <- jsonfile_data %>%
  filter(Passenger_Category == "detractor") %>%
  group_by(Partner_Name) %>%
  summarise(detractor_freq_Partner_Name = n())


Partner_Name_promoter <- jsonfile_data %>%
  filter(Passenger_Category == "promoter") %>%
  group_by(Partner_Name) %>%
  summarise(promoter_freq_Partner_Name = n())


Partner_Name_total <- jsonfile_data %>%
  group_by(Partner_Name) %>%
  summarise(total_freq_Partner_Name = n())

Partner_Name_merged = merge(Partner_Name_promoter,Partner_Name_detractor,by="Partner_Name")
Partner_Name_merged = merge(Partner_Name_total,Partner_Name_merged,by="Partner_Name")

Partner_Name_merged$detractor_Partner_Name_percent = (Partner_Name_merged$detractor_freq_Partner_Name/Partner_Name_merged$total_freq_Partner_Name)*100

Partner_Name_merged$promoter_Partner_Name_percent = (Partner_Name_merged$promoter_freq_Partner_Name/Partner_Name_merged$total_freq_Partner_Name)*100


Partner_Name_merged$Partner_Name_NPS = Partner_Name_merged$promoter_Partner_Name_percent - Partner_Name_merged$detractor_Partner_Name_percent


ggplot(Partner_Name_merged)+
  aes(x=Partner_Name,y=Partner_Name_NPS)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_col()


# ----------------- Scatter Plot -----------------

plot_data_scatter <- transform_data

# Plotting GRAPHS for Age 

plot_data_scatter %>%
  ggplot(aes(x=t_age)) +
  geom_histogram() +
  ggtitle("Histogram - Age")

plot_data_scatter %>% 
  group_by(t_age) %>%
  summarize(Average_NPS = mean(Likelihood_to_recommend)) %>%
  ggplot(aes(x=t_age,y=Average_NPS)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Scatter Plot - Age")

# Plotting GRAPHS for Price Sensitivity 

plot_data_scatter %>%
  ggplot(aes(x=Price_Sensitivity)) +
  geom_histogram() +
  ggtitle("Histogram - Price Sensitivity")

plot_data_scatter %>% 
  group_by(Price_Sensitivity) %>%
  summarize(Average_NPS = mean(Likelihood_to_recommend)) %>%
  ggplot(aes(x=Price_Sensitivity,y=Average_NPS)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Scatter Plot - Price Sensitivity")

# Plotting GRAPHS for LoyalityIndex 

plot_data_scatter %>%
  ggplot(aes(x=LoyalityIndex)) +
  geom_histogram() +
  ggtitle("Histogram - LoyalityIndex")

plot_data_scatter %>% 
  group_by(LoyalityIndex) %>%
  summarize(Average_NPS = mean(Likelihood_to_recommend)) %>%
  ggplot(aes(x=LoyalityIndex,y=Average_NPS)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Scatter Plot - LoyalityIndex")

# Plotting GRAPHS for Arrival_Delay_Greater_5 

plot_data_scatter %>%
  ggplot(aes(x=Arrival_Delay_Greater_5)) +
  geom_histogram() +
  ggtitle("Histogram - Arrival Delay Greater than 5")

plot_data_scatter %>% 
  group_by(Arrival_Delay_Greater_5) %>%
  summarize(Average_NPS = mean(Likelihood_to_recommend)) %>%
  ggplot(aes(x=Arrival_Delay_Greater_5,y=Average_NPS)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Scatter Plot - Arrival Delay Greater than 5")


# **************** REGRESSION MODEL ********************************
lm_data <- transform_data

# Regression Mgeomodel 1 - Age
RM1_age <- lm(formula = Likelihood_to_recommend ~ t_age,data = lm_data)
summary(RM1_age)

# Regression Model 2 - Price Sensitivity
RM2_PS <- lm(formula = Likelihood_to_recommend ~ Price_Sensitivity,data = lm_data)
summary(RM2_PS)

# Regression Model 3 - Loyality Index
RM3_Loyal <- lm(formula = Likelihood_to_recommend ~ LoyalityIndex,data = lm_data)
summary(RM3_Loyal)

# Regression Model 4 - Arrival Delay Greater than 5
RM4_ArrivalDelay <- lm(formula = Likelihood_to_recommend ~ Arrival_Delay_Greater_5,data = lm_data)
summary(RM4_ArrivalDelay)

# Regression Model 5 - Airline Status
RM5_status <- lm(formula = Likelihood_to_recommend ~ Airline_Status,data = lm_data)
summary(RM5_status)

# Regression Model 6 - Gender
RM6_Gender <- lm(formula = Likelihood_to_recommend ~ Gender,data = lm_data)
summary(RM6_Gender)

# Regression Model 7 - Long Flight Duration
RM7_flightduration <- lm(formula = Likelihood_to_recommend ~ Long_duration_flight,data = lm_data)
summary(RM7_flightduration)


# Regression Model 8 - class
RM8_class <- lm(formula = Likelihood_to_recommend ~ Class,data = lm_data)
summary(RM8_class)

# Regression Model 9 - Partnered Airline
RM9_Partner_Airline <- lm(formula = Likelihood_to_recommend ~ Partner_Name,data = lm_data)
summary(RM9_Partner_Airline)

# ------------------------------------------------------------------------------------------

# Regression Model A - Age + Gender
RM_A <- lm(formula=Likelihood_to_recommend ~ t_age + Gender,data = lm_data)
summary(RM_A)

# Regression Model B - Age + Gender + Price Sensitivity
RM_B <- lm(formula=Likelihood_to_recommend ~ t_age + Gender + Price_Sensitivity,data = lm_data)
summary(RM_B)

# Regression Model C - Age + Gender + Price Sensitivity + Class
RM_C <- lm(formula=Likelihood_to_recommend ~ t_age + Gender + Price_Sensitivity + Class,data = lm_data)
summary(RM_C)

# Regression Model D - Age + Gender + Price Sensitivity + Class + Airline Status
RM_D <- lm(formula=Likelihood_to_recommend ~ t_age + Gender + Price_Sensitivity + Class + Airline_Status,data = lm_data)
summary(RM_D)

# Regression Model E - Age + Gender + Price Sensitivity + Class + Airline Status + Loyality Index
RM_E <- lm(formula=Likelihood_to_recommend ~ t_age + Gender + Price_Sensitivity + Class + Airline_Status + LoyalityIndex,data = lm_data)
summary(RM_E)

# **************************************************************************************************
# OVR 1

RM_ovr1 <- lm(formula = Likelihood_to_recommend ~ t_age + Price_Sensitivity + LoyalityIndex + Arrival_Delay_Greater_5 + Airline_Status + Gender + Long_duration_flight + Class + Partner_Name,data  = lm_data)
summary(RM_ovr1)

# **************** Association Rule Mining ********************************

discrete_dataset<- jsonfile_data[c('Destination_City', 'Origin_City', 'Airline_Status', 'Gender', 'Type_of_Travel', 'Class', 'Partner_Name', 'Origin_State', 'Destination_State', 'Flight_cancelled', 'Age_Category', 'Passenger_Category')]

str(discrete_dataset)

discrete_dataset$Destination_City= as.factor(discrete_dataset$Destination_City)
discrete_dataset$Origin_City = as.factor(discrete_dataset$Origin_City)
discrete_dataset$Airline_Status=as.factor(discrete_dataset$Airline_Status)
discrete_dataset$Gender=as.factor(discrete_dataset$Gender)
discrete_dataset$Type_of_Travel=as.factor(discrete_dataset$Type_of_Travel)
discrete_dataset$Class=as.vector.factor(discrete_dataset$Class)
discrete_dataset$Partner_Name=as.factor(discrete_dataset$Partner_Name)
discrete_dataset$Origin_State=as.factor(discrete_dataset$Origin_State)
discrete_dataset$Destination_State=as.factor(discrete_dataset$Destination_State)
discrete_dataset$Flight_cancelled=as.factor(discrete_dataset$Flight_cancelled)


DATA_X <- as(discrete_dataset,"transactions")
DATA_X #Transaction Matrix

inspect(DATA_X) #Obtained 10282 rules

summary(DATA_X)

######## DETRACTOR CASE ########

ruleset1 <- apriori(DATA_X,
                    parameter=list(support=0.09,confidence=0.5),
                    appearance = list(default="lhs", rhs=("Passenger_Category=detractor")))


inspectDT(ruleset1)

summary(ruleset1)

subrules1 = ruleset1[quality(ruleset1)$confidence>0.4]

plot(subrules1,method = "paracoord")


######## PROMOTER CASE ########

ruleset2 <- apriori(DATA_X,
                    parameter=list(support=0.09,confidence=0.5),
                    appearance = list(default="lhs", rhs=("Passenger_Category=promoter")))

inspectDT(ruleset2)

summary(ruleset2)

subrules2 = ruleset2[quality(ruleset2)$confidence>0.4]

plot(subrules2,method = "paracoord")



#############Association Rule and Bar plot verify#############

#1
#Detractor
#'Class','Gender','Age_Category','Partner_Name','Passenger_Category'

dataset1<- jsonfile_data[c('Class','Gender','Age_Category','Partner_Name','Passenger_Category')]
dataset1

dataset1$Gender=as.factor(dataset1$Gender)
dataset1$Class=as.factor(dataset1$Class)
dataset1$Partner_Name=as.factor(dataset1$Partner_Name)

str(dataset1)
DATA_X1 <- as(dataset1,"transactions")
DATA_X1 #Transaction Matrix


ruleset3 <- apriori(DATA_X1,
                    parameter=list(support=0.07,confidence=0.2),
                    appearance = list(default="lhs", rhs=("Passenger_Category=detractor")))

inspectDT(ruleset3)

summary(ruleset3)


subrules3 = ruleset3[quality(ruleset3)$confidence>0.4]

###Plot#####

plot(subrules3,method = "paracoord")


#2
#Detractor
#'Partner_Name','Class', 'Passenger_Category'


dataset2<- jsonfile_data[c('Partner_Name','Class', 'Passenger_Category')]
dataset2

str(dataset2)

dataset2$Partner_Name=as.factor(dataset2$Partner_Name)
dataset2$Class=as.factor(dataset2$Class)

DATA_X3 <- as(dataset2,"transactions")
DATA_X3 #Transaction Matrix


ruleset4 <- apriori(DATA_X3,
                    parameter=list(support=0.02,confidence=0.25),
                    appearance = list(default="lhs", rhs=("Passenger_Category=detractor")))

inspectDT(ruleset4)

summary(ruleset4)

subrules4 = ruleset4[quality(ruleset4)$confidence>0.3]

###Plot#####

plot(subrules4,method = "paracoord")



#3
#promoter
#'Class','Gender','Age_Category','Partner_Name','Passenger_Category'

dataset3<- jsonfile_data[c('Class','Gender','Age_Category','Partner_Name','Passenger_Category')]
dataset3

dataset3$Gender=as.factor(dataset3$Gender)
dataset3$Class=as.factor(dataset3$Class)
dataset3$Partner_Name=as.factor(dataset3$Partner_Name)

str(dataset3)
DATA_X3 <- as(dataset1,"transactions")
DATA_X3 #Transaction Matrix


ruleset5 <- apriori(DATA_X3,
                    parameter=list(support=0.07,confidence=0.2),
                    appearance = list(default="lhs", rhs=("Passenger_Category=promoter")))

inspectDT(ruleset5)

summary(ruleset5)


subrules5 = ruleset5[quality(ruleset5)$confidence>0.4]

###Plot#####

plot(subrules5,method = "paracoord")


#4
#promoter
#'Partner_Name','Class', 'Passenger_Category'


dataset4<- jsonfile_data[c('Partner_Name','Class', 'Passenger_Category')]
dataset4

str(dataset4)

dataset4$Partner_Name=as.factor(dataset4$Partner_Name)
dataset4$Class=as.factor(dataset4$Class)

DATA_X4 <- as(dataset4,"transactions")
DATA_X4 #Transaction Matrix


ruleset6 <- apriori(DATA_X4,
                    parameter=list(support=0.02,confidence=0.25),
                    appearance = list(default="lhs", rhs=("Passenger_Category=promoter")))

inspectDT(ruleset6)

summary(ruleset6)

subrules6 = ruleset6[quality(ruleset6)$confidence>0.3]

###Plot#####

plot(subrules6,method = "paracoord")


# **************** Support Vector Models ********************************


trainindex <- sample(c(1,2,3), nrow(transform_data),replace= T,prob = c(0.15,0.45,0.4))
traindata <- transform_data[trainindex==1,]
testdata <- transform_data[trainindex==2,]
svmOutput <- ksvm(Likelihood_to_recommend ~ Airline_Status + Age +  Price_Sensitivity +
                    Flight_Distance +  Type_of_Travel +  Flight_cancelled +
                    Shopping_Amount_at_Airport + Class + 
                    Departure_Delay_in_Minutes + Arrival_Delay_in_Minutes,
                  data=traindata,kernel="rbfdot", kpar="automatic",C=20,cross=10, prob.model=TRUE)
svmOutput

#View(testdata)
svmresult <- predict(svmOutput,testdata,type="votes")
#View(svmresult)
compactable <- (testdata[,27]- svmresult)<10&(testdata[,27]-svmresult) > 0
result <- table(compactable)
result
accuraryratio <- result[2]/(sum(result))
accuraryratio
#accuracy ratio: 0.5214379

