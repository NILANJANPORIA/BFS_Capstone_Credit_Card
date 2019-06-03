# Cleaning up the environment
remove (list=ls())

#set working directory
#setwd("F:/PGDDS/Capstone_Project")

###########################
######### Importing libraries
###########################
library(dplyr)
library (ggplot2)
library(fuzzyjoin)
library(stringr)
library(randomForest)
library(Information)
library(DMwR)
library(cowplot)
library(MASS)
library(car)
library(caret)
library(caTools)
library(dummies)
library(xgboost)
library(ROCR)


###########################
######### Importing dataset
###########################
Credit_Bureau_Data <- read.csv("Credit Bureau data.csv",stringsAsFactors = FALSE,na.strings=c("", " ", "NA", "N/A"))
Demographic_Data <- read.csv("Demographic data.csv",stringsAsFactors = FALSE,na.strings=c("", " ", "NA", "N/A"))
###########################



##############################################################################
###
### Data Understanding & preparation
###
##############################################################################

#------------------------------------------------------------------------------------------------
###############
## Key Observation 1:
##                  1: No duplicate records available in "Credit Bureau Data" and "Demographic data" 
##                  2: --------------------No duplicate records available in given data set
###############

#Step 1: (Understanding: No duplicate records present in datasets)
sum(duplicated(Credit_Bureau_Data))   #count: 0
sum(duplicated(Demographic_Data))     #count: 0


## Step 2 (Observing attributes of data)
str(Credit_Bureau_Data)  
str(Demographic_Data)

#------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------
###############
## Key Observation 2:
##                  1: "Application ID" is the primary key
##                      Both "Credit Bureau Data" and "Demographic data" contain 71295 "Application ID" where 3 "Application ID" have duplicate.
##                  2: both "Credit Bureau Data" and "Demographic data" contain information of same "Application Id"
##
## Data Cleaning:
##                 Reconds containing duplicate "Application ID" (bad records) removed as number is very very less 
###############

#Step 1: (Understanding: No "Application ID" is null in datasets)
sum(is.na(Credit_Bureau_Data$Application.ID)) #count: 0
sum(is.na(Demographic_Data$Application.ID))   #count: 0

#Step 2: (Understanding: "Application ID" has duplicate for 3 records in both "Credit Bureau Data" and "Demographic data")
length(Credit_Bureau_Data$Application.ID)  #count: 71295
length(unique(Credit_Bureau_Data$Application.ID)) #count: 71292

length(Demographic_Data$Application.ID)  #count: 71295
length(unique(Demographic_Data$Application.ID)) #count: 71292

## Step 3: (understanding: 3 duplicated "Application.ID" (values 765011468, 653287861 ,671989187) are common in both datasets

Dupl_app_id_CBdata <- Credit_Bureau_Data[duplicated(Credit_Bureau_Data$Application.ID),'Application.ID']
## Answer: 765011468 653287861 671989187

Dupl_app_id_DGdata <-Demographic_Data[duplicated(Demographic_Data$Application.ID),'Application.ID']
## Answer: 765011468 653287861 671989187

## Credit Bureau and Demographic Data is different for duplicate records
Credit_Bureau_Data[Credit_Bureau_Data$Application.ID %in% Dupl_app_id_CBdata,]
Demographic_Data[Demographic_Data$Application.ID %in% Dupl_app_id_DGdata,]

## Clearly Application Id can't be duplicated, so this 3 is bad records.
## As total no of bad records are very less w.r.t. available data, so those records can be removed.

#--Data Cleaning--
# remove all the bad records from dataset(as no of records is less)
Credit_Bureau_Data <- Credit_Bureau_Data[!(Credit_Bureau_Data$Application.ID %in% Dupl_app_id_CBdata),]
Demographic_Data <- Demographic_Data[!(Demographic_Data$Application.ID %in% Dupl_app_id_DGdata),]

length(unique(Credit_Bureau_Data$Application.ID)) #Count: 71289
length(Demographic_Data$Application.ID)           #Count: 71289


## Step 4  (understanding: both "Credit Bureau Data" and "Demographic data" contain same Application Id
setdiff(Credit_Bureau_Data$Application.ID,Demographic_Data$Application.ID)
#------------------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------------
###############
## Key Observation 3:
##                  1: "Performance.Tag" has values 0,1 or null
##                      "Performance.Tag" null means those applications has rejected hence Performance can't be measured.
##                  2:  6917 applications performance is good, but 2947 applications are defaulters.
##
## Data Cleaning:
##                 Reconds containing null "Performance.Tag" removed as those applications are not useful for this analysis
###############

#Step 1: (Understanding: "Performance.Tag" has null value in datasets)
sum(is.na(Credit_Bureau_Data$Performance.Tag)) #count: 1425
sum(is.na(Demographic_Data$Performance.Tag))   #count: 1425
# Cleaning Step 1: "Performance.Tag" null means those applications has rejected hence Performance can't be measured
#       As there is no other ways availbale (like predicting their performance using by different bank application details)
#       Those records are filtered out

#Application_Demographic_Data<-Demographic_Data
#Application_Credit_Bureau_Data<-Credit_Bureau_Data

#Demographic_Data<-Demographic_Data[which(!sapply(Demographic_Data$Performance.Tag,is.na)),]
#Credit_Bureau_Data<-Credit_Bureau_Data[which(!sapply(Credit_Bureau_Data$Performance.Tag,is.na)),]

#Step 2: (Understanding: "Performance.Tag" values can be 0 or 1 i.e. aligned with bussiness understanding )
unique(Credit_Bureau_Data$Performance.Tag)
unique(Demographic_Data$Performance.Tag)
# Cleaning Step 2: "Performance.Tag" should be a factor
Credit_Bureau_Data$Performance.Tag<-factor(Credit_Bureau_Data$Performance.Tag)
Demographic_Data$Performance.Tag<-factor(Demographic_Data$Performance.Tag)

#Step 3: (Understanding: for 66917 applications performance is good, but 2947 applications are defaulters)
summary(Credit_Bureau_Data$Performance.Tag)
summary(Demographic_Data$Performance.Tag)


#------------------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------------
#--------------- Attribute related to DPD ('Days Past Due') on 'Credit Bureau Data'--------------

#Step 1: Checking the values and frequency for DPD attributes 
# Observation: No Null values are present
summary(factor(Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.6.months))
summary(factor(Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.6.months))
summary(factor(Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.6.months))

summary(factor(Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.12.months))
summary(factor(Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.12.months))
summary(factor(Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.12.months))

# Step 2: Visualizing the outliers in DPD attributes
boxplot(Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.6.months)
boxplot(Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.6.months)
boxplot(Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.6.months)

boxplot(Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.12.months)
boxplot(Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.12.months)
boxplot(Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.12.months)

# Outlier treatment for DPD attributes
quantile(Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.6.months,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.6.months[which(Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.6.months > 2)]<- 2

quantile(Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.6.months,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.6.months[which(Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.6.months > 3)]<- 3

quantile(Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.6.months,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.6.months[which(Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.6.months > 4)]<- 4

quantile(Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.12.months[which(Credit_Bureau_Data$No.of.times.90.DPD.or.worse.in.last.12.months > 3)]<- 3

quantile(Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.12.months[which(Credit_Bureau_Data$No.of.times.60.DPD.or.worse.in.last.12.months > 4)]<- 4

quantile(Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.12.months[which(Credit_Bureau_Data$No.of.times.30.DPD.or.worse.in.last.12.months > 5)]<- 5

#------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------
#---------------"Avgas CC Utilization in last 12 months" Attribute on 'Credit Bureau Data'--------------

###### Question 1: Should we ignore when count is 113

#Step 1: Null checking "Avgas CC Utilization in last 12 months"
sum(is.na(Credit_Bureau_Data$Avgas.CC.Utilization.in.last.12.months))
# Observation: Null values are there, as count is significant, this should be handled using woe

summary(Credit_Bureau_Data$Avgas.CC.Utilization.in.last.12.months)

length(which(Credit_Bureau_Data$Avgas.CC.Utilization.in.last.12.months<113))
boxplot(Credit_Bureau_Data$Avgas.CC.Utilization.in.last.12.months)

quantile(Credit_Bureau_Data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)




#------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------
#---------------"Trade information" Attributes on 'Credit Bureau Data'--------------

#Step 1: Checking Null values
sum(is.na(Credit_Bureau_Data$No.of.trades.opened.in.last.6.months))
sum(is.na(Credit_Bureau_Data$No.of.trades.opened.in.last.12.months))
sum(is.na(Credit_Bureau_Data$Total.No.of.Trades))
#Observation: only one Null value is present for "No.of.trades.opened.in.last.6.months" attribute,
#             hence that value can be removed (as number of records are huge)
Credit_Bureau_Data<-Credit_Bureau_Data[which(!sapply(Credit_Bureau_Data$No.of.trades.opened.in.last.6.months,is.na)),]

# Step 2: Visualizing the outliers
boxplot(Credit_Bureau_Data$No.of.trades.opened.in.last.6.months)
boxplot(Credit_Bureau_Data$No.of.trades.opened.in.last.12.months)
boxplot(Credit_Bureau_Data$Total.No.of.Trades)

# Step 3: Outlier treatment
quantile(Credit_Bureau_Data$No.of.trades.opened.in.last.6.months,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.trades.opened.in.last.6.months[which(Credit_Bureau_Data$No.of.trades.opened.in.last.6.months > 9)]<- 9

quantile(Credit_Bureau_Data$No.of.trades.opened.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.trades.opened.in.last.12.months[which(Credit_Bureau_Data$No.of.trades.opened.in.last.12.months > 21)]<- 21

quantile(Credit_Bureau_Data$Total.No.of.Trades,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$Total.No.of.Trades[which(Credit_Bureau_Data$Total.No.of.Trades > 31)]<- 31



#------------------------------------------------------------------------------------------------
#--------------- Attribute related to "Account Inquiry" & "Loan" on 'Credit Bureau Data'--------------
#Step 1: Checking Null values
sum(is.na(Credit_Bureau_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))   # Count 0
sum(is.na(Credit_Bureau_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))  # Count 0
sum(is.na(Credit_Bureau_Data$Presence.of.open.auto.loan))  # Count 0
sum(is.na(Credit_Bureau_Data$Presence.of.open.home.loan))  # Count 272
#Observation: Home Loan has 272 null values, as the count is relatively significant, those will be handle by woe

# Step 2: Visualizing the outliers for numeric columns
boxplot(Credit_Bureau_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
boxplot(Credit_Bureau_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)


# Step 3: Outlier treatment for numeric columns
quantile(Credit_Bureau_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(Credit_Bureau_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. > 8)]<- 8

quantile(Credit_Bureau_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,seq(0,1,0.01),na.rm = TRUE)
Credit_Bureau_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(Credit_Bureau_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. > 15)]<- 15

# Step 4: Convert factor for catagorical columns
summary(factor(Credit_Bureau_Data$Presence.of.open.auto.loan))
summary(factor(Credit_Bureau_Data$Presence.of.open.home.loan))
Credit_Bureau_Data$Presence.of.open.auto.loan<-factor(Credit_Bureau_Data$Presence.of.open.auto.loan)
Credit_Bureau_Data$Presence.of.open.home.loan<-factor(Credit_Bureau_Data$Presence.of.open.home.loan)
str(Credit_Bureau_Data)



#------------------------------------------------------------------------------------------------
#--------------- Attribute related to "Outstanding Balance" on 'Credit Bureau Data'--------------
#Step 1: Checking Null values
sum(is.na(Credit_Bureau_Data$Outstanding.Balance)) # 272
#Observation: Home Loan has 272 null values, as the count is relatively significant, those will be handle by woe


# Step 2: Visualizing the outliers
boxplot(Credit_Bureau_Data$Outstanding.Balance)
# observation: No outlier found, so no outlier treatment needed

#------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------
#--------------- Numerical attributes (age,"No of dependents","No of months in current residence",
# income,"No of months in current company") on 'Demographic Data' ----------------------------
#Step 1: Checking Null values
sum(is.na(Demographic_Data$Age)) # Count 0
sum(is.na(Demographic_Data$No.of.dependents)) # Count 3
sum(is.na(Demographic_Data$Income)) # Count 0
sum(is.na(Demographic_Data$No.of.months.in.current.residence)) # Count 0
sum(is.na(Demographic_Data$No.of.months.in.current.company)) # Count 0
# Observation: "No of dependents" has 3 null values, as this is very few records it can be deleted
Demographic_Data<-Demographic_Data[which(!sapply(Demographic_Data$No.of.dependents,is.na)),]


# Step 2: Visualizing the outliers for Numerical attributes
boxplot(Demographic_Data$Age) #Outlier found , even for few applicants age is 0 or lessi.e. bad records availbles
boxplot(Demographic_Data$No.of.dependents)    # No outlier found
boxplot(Demographic_Data$Income)          # No outlier found
boxplot(Demographic_Data$No.of.months.in.current.residence)          # No outlier found
boxplot(Demographic_Data$No.of.months.in.current.company)          # outlier found

# Step 3: Bad record treatment for "Income" and "Age" attributes
summary(Demographic_Data$Income) # Negative Income is available, what is impractical
summary(Demographic_Data$Age)   # Negative Age is available, what is wrong
summary(Demographic_Data$No.of.dependents)
summary(Demographic_Data$No.of.months.in.current.residence)
summary(Demographic_Data$No.of.months.in.current.company)

# 
length(which(Demographic_Data$Age<=0)) # 19 bad records found
Demographic_Data<-Demographic_Data[which(Demographic_Data$Age>0),]

length(which(Demographic_Data$Income<0)) # 81 bad records found
Demographic_Data<-Demographic_Data[which(Demographic_Data$Income>=0),]

# Step 4: Outlier treatment for "Age" and "No.of.months.in.current.company" attributes

quantile(Demographic_Data$Age,seq(0,1,0.01),na.rm = TRUE)
Demographic_Data$Age[which(Demographic_Data$Age < 18)]<- 18 # Based on Bussiness understnading

quantile(Demographic_Data$No.of.months.in.current.company,seq(0,1,0.01),na.rm = TRUE)
Demographic_Data$No.of.months.in.current.company[which(Demographic_Data$No.of.months.in.current.company > 74)]<- 74


#------------------------------------------------------------------------------------------------
#--------------- Catagorical attributes (Gender,Marital Status,Education,Profession,Residence Type) on 'Demographic Data' ----------------------------

summary(factor(Demographic_Data$Gender))  # Values: Male Female and Null (2 records)
summary(factor(Demographic_Data$Marital.Status..at.the.time.of.application.))  # Values: Married Single and Null (6 records)
summary(factor(Demographic_Data$Education)) # VAlues: Bachelor, Masters, Others, Phd , Professional  118 null values
summary(factor(Demographic_Data$Profession)) # Values : SAL, SE, SE_PROF and 13 Null values
summary(factor(Demographic_Data$Type.of.residence)) #Values: Company Provided, Living with PArents, Others, Owned, Rented and 8 Null values
#Observation:
# Although count is less  but Null values are available all these columns.
# However Education has relatively higher null values, so that will be taken care by woe
# For other attibutes, we can remove null values
Demographic_Data<-Demographic_Data[which(!sapply(Demographic_Data$Gender,is.na)),]
Demographic_Data<-Demographic_Data[which(!sapply(Demographic_Data$Marital.Status..at.the.time.of.application.,is.na)),]
Demographic_Data<-Demographic_Data[which(!sapply(Demographic_Data$Profession,is.na)),]
Demographic_Data<-Demographic_Data[which(!sapply(Demographic_Data$Type.of.residence,is.na)),]

# Step4 : Converting factor for Catagorical attributes
Demographic_Data$Gender<-factor(Demographic_Data$Gender)
Demographic_Data$Marital.Status..at.the.time.of.application.<-factor(Demographic_Data$Marital.Status..at.the.time.of.application.)
Demographic_Data$Profession<-factor(Demographic_Data$Profession)
Demographic_Data$Education<-factor(Demographic_Data$Education)
Demographic_Data$Type.of.residence<-factor(Demographic_Data$Type.of.residence)

### Merging two dataset 
str(Demographic_Data)
str(Credit_Bureau_Data)
summary(Demographic_Data$Performance.Tag)
summary(Credit_Bureau_Data$Performance.Tag)

Bank_Data <- merge(Demographic_Data,Credit_Bureau_Data,by="Application.ID")
all(Bank_Data$Performance.Tag.x == Bank_Data$Performance.Tag.y) # True
#Removing Duplicate "Performance.Tag"
Bank_Data<- subset(Bank_Data, select=-c(Performance.Tag.x))
names(Bank_Data)[names(Bank_Data) == 'Performance.Tag.y'] <- 'Performance.Tag'

str(Bank_Data)

#View(Bank_Data)
#___________________________________________________________________________________________________________

# ##Frequency distribution##
# 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(Bank_Data, aes(x=as.factor(Age),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill"), 
          ggplot(Bank_Data, aes(x=as.factor(Gender),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Marital.Status..at.the.time.of.application.),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.dependents),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Income),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,align = "h")

plot_grid(ggplot(Bank_Data, aes(x=as.factor(Education),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Profession),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Type.of.residence),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.months.in.current.residence),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.months.in.current.company),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,align = "h")

plot_grid(ggplot(Bank_Data, aes(x=as.factor(No.of.times.90.DPD.or.worse.in.last.6.months),fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.times.60.DPD.or.worse.in.last.6.months),fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.times.30.DPD.or.worse.in.last.6.months),fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.times.90.DPD.or.worse.in.last.12.months),fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.times.60.DPD.or.worse.in.last.12.months),fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,align = "h")

plot_grid(ggplot(Bank_Data, aes(x=as.factor(No.of.times.30.DPD.or.worse.in.last.12.months),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Avgas.CC.Utilization.in.last.12.months),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.trades.opened.in.last.6.months),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.trades.opened.in.last.12.months),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,align = "h")

plot_grid(ggplot(Bank_Data, aes(x=as.factor(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Presence.of.open.home.loan),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Outstanding.Balance),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Total.No.of.Trades),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Bank_Data, aes(x=as.factor(Presence.of.open.auto.loan),fill=as.factor(Performance.Tag)))+ geom_bar(position = "fill")+bar_theme1,align = "h") 

#############################
# Bivariate analysis 
ggplot(Bank_Data, aes(Age)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(Gender)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(Marital.Status..at.the.time.of.application.)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(Education)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(Profession)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(Type.of.residence)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(No.of.months.in.current.residence)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(No.of.months.in.current.company)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(Outstanding.Balance)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(No.of.months.in.current.company)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(No.of.trades.opened.in.last.12.months)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(Avgas.CC.Utilization.in.last.12.months)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()
ggplot(Bank_Data, aes(Presence.of.open.auto.loan)) +  geom_bar() +  facet_grid(~Performance.Tag) +  coord_flip()


################################  Multiverate analysis  ################################

## Creating Correlation Matrix
Bank_Data_cont_attr_name<-c("Age","No.of.dependents","Income","No.of.months.in.current.residence","No.of.months.in.current.company",
                            "No.of.times.90.DPD.or.worse.in.last.6.months","No.of.times.60.DPD.or.worse.in.last.6.months",
                            "No.of.times.30.DPD.or.worse.in.last.6.months","No.of.times.90.DPD.or.worse.in.last.12.months",
                            "No.of.times.60.DPD.or.worse.in.last.12.months","No.of.times.30.DPD.or.worse.in.last.12.months",
                            "Avgas.CC.Utilization.in.last.12.months","No.of.trades.opened.in.last.6.months",
                            "No.of.trades.opened.in.last.12.months","No.of.PL.trades.opened.in.last.6.months",
                            "No.of.PL.trades.opened.in.last.12.months","No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                            "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.","Outstanding.Balance","Total.No.of.Trades")

Bank_Data_cont_attr<-Bank_Data[,Bank_Data_cont_attr_name]

Cor_Matrix_Bank_Data_cont_attr <- cor(Bank_Data_cont_attr)

write.csv(round(Cor_Matrix_Bank_Data_cont_attr, 2), file = "Corr_Matrix_Final.csv")




#___________________________________________________________________________________________________________

## Separating Data frame for Application Scorecard and Credit Card Model
Application_Demographic_Data<-Demographic_Data
Application_Credit_Bureau_Data<-Credit_Bureau_Data
Application_Bank_Data<-Bank_Data

Demographic_Data<-Demographic_Data[which(!sapply(Demographic_Data$Performance.Tag,is.na)),]
Credit_Bureau_Data<-Credit_Bureau_Data[which(!sapply(Credit_Bureau_Data$Performance.Tag,is.na)),]
Bank_Data<-Bank_Data[which(!sapply(Bank_Data$Performance.Tag,is.na)),]


# Swapping the defaultes and good guys 
# 1 Non defaulters and 0 as Defaulters
Bank_Data$Performance.Tag <- ifelse(Bank_Data$Performance.Tag == 0,1,0)
summary(factor(Bank_Data$Performance.Tag))

#Bank_Data$Performance.Tag <- ifelse(Bank_Data$Performance.Tag == 0,1,0)
#----------------WOE------------------------------

IV <- create_infotables(data=Bank_Data, 
                        y="Performance.Tag",
                        bins = 10,
                        parallel=FALSE)
IV_Value = data.frame(IV$Summary)

IV_Value[IV_Value$IV<0.02 , "Var_Pred"] <- "Not useful for prediction"
IV_Value[IV_Value$IV >= 0.02 & IV_Value$IV < 0.1 , "Var_Pred"] <- "Weak predictive Power"
IV_Value[IV_Value$IV >= 0.1 & IV_Value$IV < 0.3 , "Var_Pred"] <- "Medium predictive Power"
IV_Value[IV_Value$IV >= 0.3 & IV_Value$IV < 0.5 , "Var_Pred"] <- "Strong predictive Power"
IV_Value[IV_Value$IV >= 0.5 , "Var_Pred"] <- "Suspicious Predictive Power"

IV_Value


### Below function used for replacing actual values with WOE value
woe_replace <- function(df, IV,skip_columns)
{
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  # identifying the column names and type of dataframe
  df_Column_Info <- data.frame(column_name = colnames(df),column_type = sapply(df, class))
  
  # for each and every Column below operations are done
  for (rownm in 1:nrow(df_Column_Info))
  {
    # choosing a column for operation
    colmn_nm <- toString(df_Column_Info[rownm, "column_name"])
    
    # if that dataframe column is present in IV and not presnt in skip_columns then perform below operation
    if(colmn_nm %in% names(IV$Tables) & colmn_nm %!in% skip_columns)
    {
      column_woe_df<-cbind(data.frame(IV$Tables[[colmn_nm]]))
      
      # if Column Type is Character / Factor, perform below operation
      # inner join with IV and actual dataframe based on "Column Name" and IV values. Keep only WOE values and delete actual values
      if (df_Column_Info[rownm, "column_type"] == "factor" | df_Column_Info[rownm, "column_type"] == "character") 
      {
        df <- dplyr::inner_join(
          df,
          column_woe_df[,c(colmn_nm,"WOE")],
          by = colmn_nm,
          type = "inner",
          match = "all"
        )
        df[colmn_nm]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      }
      
      
      # if Column Type is Integer / Numeric, perform below operation
      # fuzzy inner join with IV and actual dataframe based on "Column Name" and lower and upper boundary of bin (derived from IV value)
      # Keep only WOE values and delete actual values
      else if (df_Column_Info[rownm, "column_type"] == "numeric" | df_Column_Info[rownm, "column_type"] == "integer")
      {
        column_woe_df$lv<-as.numeric(str_sub(column_woe_df[,colmn_nm],regexpr("\\[", column_woe_df[,colmn_nm]) + 1,regexpr(",", column_woe_df[,colmn_nm]) - 1))
        column_woe_df$uv<-as.numeric(str_sub(column_woe_df[,colmn_nm],regexpr(",", column_woe_df[,colmn_nm]) + 1,regexpr("\\]", column_woe_df[,colmn_nm]) - 1))
        column_woe_df[colmn_nm]<-NULL 
        column_woe_df<-column_woe_df[,c("lv","uv","WOE")]
        colnames(df)[colnames(df)==colmn_nm]<-"Dummy_Column_Name"
        df$Dummy_Column_Name<-as.numeric(df$Dummy_Column_Name)
        
        df <-fuzzy_inner_join(df,column_woe_df,
                              by = c("Dummy_Column_Name"="lv","Dummy_Column_Name"="uv"),match_fun=list(`>=`,`<=`))
        
        df["Dummy_Column_Name"]<-NULL      
        df["lv"]<-NULL      
        df["uv"]<-NULL      
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm 
      }
      
    }
  }
  
  return(df)
}

Bank_Data_woe <- woe_replace(Bank_Data,IV,c("Application.ID"))
#View(Bank_Data_woe)



######################################################################################################
######################### Model building and evaluation for Entire Bank Data #########################
######################################################################################################

# splitting the data between train and test
set.seed(123)

indices = sample.split(Bank_Data_woe$Performance.Tag, SplitRatio = 0.7)

train_bank = Bank_Data_woe[indices,]
train_bank_bkp = train_bank
test_bank = Bank_Data_woe[!(indices),]
test_bank_bkp = test_bank


##########################################################################
#_______________Derived Trained Data as number of defaulers is very less______________
summary(factor(train_bank$Performance.Tag))

## So number of defaulters is very less around 4.2% of data, hence the data is very unbalanced and biased to "Good Customers"

train_bank$Performance.Tag<-factor(train_bank$Performance.Tag)
train_bank_smoted <- SMOTE("Performance.Tag"~., train_bank, perc.over=300,perc.under=150, seed = 100)

#train_bank_smoted <- SMOTE("Performance.Tag"~., train_bank, perc.over=100,perc.under=200)
train_bank_smoted_bkp = train_bank_smoted
summary(train_bank_smoted$Performance.Tag)
# Number of good Customers: 9126 , Number of Defaulters = 8112 
# Now number of defaultes (Represents by Performance.Tag= 0) is significant (47%)





#____________________________________________________________________________________________________#
######################################################################################################
######################## MODEL 1: Logistic Regression: (for entire Bank Data) ########################
######################################################################################################
#____________________________________________________________________________________________________#


############ Logistic Model Creation for entire Bank Data ##############
########################################################################
set.seed(123)

# As column numbers are relatively huge, so only Strong, Medium and Weak predictive variables need to be considered
# based on IV values on below columns are choosen

imp_columns <- c("Avgas.CC.Utilization.in.last.12.months",	"No.of.trades.opened.in.last.12.months",	"No.of.PL.trades.opened.in.last.12.months",	"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",	"Outstanding.Balance",	"No.of.times.30.DPD.or.worse.in.last.6.months",	"Total.No.of.Trades",	"No.of.PL.trades.opened.in.last.6.months",	"No.of.times.90.DPD.or.worse.in.last.12.months",	"No.of.times.60.DPD.or.worse.in.last.6.months",	"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",	"No.of.times.30.DPD.or.worse.in.last.12.months",	"No.of.trades.opened.in.last.6.months",	"No.of.times.60.DPD.or.worse.in.last.12.months",	"No.of.times.90.DPD.or.worse.in.last.6.months",	"No.of.months.in.current.residence",	"Income",	"No.of.months.in.current.company","Performance.Tag")

train_bank_smoted <- train_bank_smoted[,imp_columns]
train_bank_smoted_imp_col_bkp = train_bank_smoted

### Model Evaluation
logistic_bank_model_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_1)

# many columns found insignificant
# Using stepwise algorithm for removing insignificant variables 
logistic_bank_model_2 <- stepAIC(logistic_bank_model_1, direction="both")
summary(logistic_bank_model_2)
vif(logistic_bank_model_2)

# removing "No.of.trades.opened.in.last.6.months "
logistic_bank_model_3 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                               No.of.trades.opened.in.last.12.months + 
                               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                               Outstanding.Balance + 
                               No.of.times.30.DPD.or.worse.in.last.6.months + 
                               Total.No.of.Trades + 
                               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                               No.of.times.60.DPD.or.worse.in.last.12.months + 
                               No.of.times.90.DPD.or.worse.in.last.6.months + 
                               No.of.months.in.current.residence + 
                               Income
                             , family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_3)
vif(logistic_bank_model_3)

# removing "No.of.months.in.current.residence  "
logistic_bank_model_4 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                               No.of.trades.opened.in.last.12.months + 
                               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                               Outstanding.Balance + 
                               No.of.times.30.DPD.or.worse.in.last.6.months + 
                               Total.No.of.Trades + 
                               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                               No.of.times.60.DPD.or.worse.in.last.12.months + 
                               No.of.times.90.DPD.or.worse.in.last.6.months + 
                               Income
                             , family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_4)
vif(logistic_bank_model_4)


# removing "No.of.times.60.DPD.or.worse.in.last.12.months"
logistic_bank_model_5 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                               No.of.trades.opened.in.last.12.months + 
                               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                               Outstanding.Balance + 
                               No.of.times.30.DPD.or.worse.in.last.6.months + 
                               Total.No.of.Trades + 
                               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                               No.of.times.90.DPD.or.worse.in.last.6.months + 
                               Income
                             , family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_5)
vif(logistic_bank_model_5)


# removing "No.of.times.90.DPD.or.worse.in.last.6.months"
logistic_bank_model_6 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                               No.of.trades.opened.in.last.12.months + 
                               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                               Outstanding.Balance + 
                               No.of.times.30.DPD.or.worse.in.last.6.months + 
                               Total.No.of.Trades + 
                               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                               Income
                             , family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_6)
vif(logistic_bank_model_6)

# removing "Income"
logistic_bank_model_7 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                               No.of.trades.opened.in.last.12.months + 
                               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                               Outstanding.Balance + 
                               No.of.times.30.DPD.or.worse.in.last.6.months + 
                               Total.No.of.Trades + 
                               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
                             , family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_7)
vif(logistic_bank_model_7)


# removing "No.of.trades.opened.in.last.12.months"
logistic_bank_model_8 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                               Outstanding.Balance + 
                               No.of.times.30.DPD.or.worse.in.last.6.months + 
                               Total.No.of.Trades + 
                               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
                             , family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_8)
vif(logistic_bank_model_8)


# removing "Total.No.of.Trades"
logistic_bank_model_9 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                               Outstanding.Balance + 
                               No.of.times.30.DPD.or.worse.in.last.6.months + 
                               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
                             , family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_9)
vif(logistic_bank_model_9)


# removing "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."
logistic_bank_model_10 <- glm(formula = Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                Outstanding.Balance + 
                                No.of.times.30.DPD.or.worse.in.last.6.months
                              , family = "binomial", data = train_bank_smoted)
summary(logistic_bank_model_10)
vif(logistic_bank_model_10)


final_model_bank_logistic=logistic_bank_model_10

########################################################################
######## 1. B >> Logistic Model Validation for entire Bank Data ########
########################################################################

test_bank_logistic_pred = predict(final_model_bank_logistic, type = "response", 
                                  newdata = test_bank[,-2])  #second column is performance tag

# Summary of prediction
summary(test_bank_logistic_pred)


test_bank$prob <- test_bank_logistic_pred
#View(test_bank)

#____________________________________________
###Model Evalution (assuming cutoff is 0.50)
## Checking model assuming cutoff is 0.50 (i.e. if "prediction value" <= 0.50 then Performance.Tag= 0)
test_bank_pred_tag <- factor(ifelse(test_bank_logistic_pred <= 0.50, "Yes", "No"))
test_bank_actual_tag <- factor(ifelse(test_bank$Performance.Tag==0,"Yes","No"))
table(test_bank_actual_tag,test_bank_pred_tag)
test_bank_conf <- confusionMatrix(test_bank_pred_tag, test_bank_actual_tag, positive = "Yes")
test_bank_conf
#Accuracy 60% , Sensitivity 66% , Specificity is nearly 59.7%
#____________________________________________

#____________________________________________
## Cut Off Value calculation
#____________________________________________
summary(test_bank_logistic_pred)
# Creating cutoff values from 0.3131  (Min Value) to 0.7953  (Max Value) for plotting and initiallizing a matrix of 100 X 3.
# Summary of test probability
s = seq(0.3131 ,0.7953,length=100)

############## Below function predict "sensitivity", "specificity", "accuracy" for a certain cutoff value
## optimal cutoff identification
perform_prob_cutoff_fn <- function(cutoff,test_actual,test_pred) 
{
  predicted_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

OUT_BANK_LOGISTIC = matrix(0,100,3)

test_bank_actual_tag_logistic <- factor(ifelse(test_bank$Performance.Tag==0,"Yes","No"))
for(i in 1:100)
{
  OUT_BANK_LOGISTIC[i,] = perform_prob_cutoff_fn(s[i],test_bank_actual_tag_logistic,test_bank$prob)
} 
OUT_BANK_LOGISTIC

cutoff_val <- min(abs(OUT_BANK_LOGISTIC[,1]-OUT_BANK_LOGISTIC[,2]))


plot(s, OUT_BANK_LOGISTIC[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_BANK_LOGISTIC[,2],col="darkgreen",lwd=2)
lines(s,OUT_BANK_LOGISTIC[,3],col="blue",lwd=2)
box()
legend(.5,.5,col=c(2,"darkgreen","blue","darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))



cutoff_final <- s[which(abs(OUT_BANK_LOGISTIC[,1]-OUT_BANK_LOGISTIC[,2])==cutoff_val)]
cutoff_final
cutoff_final_logistic <-cutoff_final

# predicting test record using derived cutoff value on  final model
test_bank_pred_tag_logistic <- factor(ifelse(test_bank$prob <=cutoff_final, "Yes","No"))
table(test_bank_actual_tag_logistic,test_bank_pred_tag_logistic)
conf_final <- confusionMatrix(test_bank_pred_tag_logistic, test_bank_actual_tag_logistic, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

## Accuracy = 62.3% / Sensitivity = 64.3 % / Specificity = 62.2 %
##################################################################################################
### KS -statistic - Test Data ######
summary(test_bank_pred_tag_logistic)
summary(test_bank_actual_tag_logistic)
predicted_response_bank_logistic <- ifelse(test_bank_pred_tag_logistic=="Yes",1,0)
actual_response_logistic_bank <- ifelse(test_bank_actual_tag_logistic=="Yes",1,0)

pred_object_bank_test_logistic<- prediction(as.numeric(predicted_response_bank_logistic), as.numeric(actual_response_logistic_bank))
bank_logistic_performance_measures_test<- performance(pred_object_bank_test_logistic, "tpr", "fpr")


bank_logistic_ks_table_test <- attr(bank_logistic_performance_measures_test, "y.values")[[1]] - 
  (attr(bank_logistic_performance_measures_test, "x.values")[[1]])
max(bank_logistic_ks_table_test) # 0.2658055

#KS-statistic for Bank Data (using Logistic Regression) is 26%

#ROC Curve

bank_logistic_auc_ROCR <- performance(pred_object_bank_test_logistic, measure = "auc")
bank_logistic_auc_ROCR <- bank_logistic_auc_ROCR@y.values[[1]]
bank_logistic_auc_ROCR 
## # Area under curve for Bank Data (using Logistic Regression) is : 0.6329027


pd_bank_logistic <- data.frame(fpr=unlist(bank_logistic_performance_measures_test@x.values), tpr=unlist(bank_logistic_performance_measures_test@y.values))

## ROC Curve for Logistic Regression
ggplot(pd_bank_logistic ,aes(x=fpr, y=tpr)) +
  geom_line(colour="blue") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Logistic Regression") +
  annotate("text", x=0.4, y=0.00, size=5,label=paste("AUC  =", round(bank_logistic_auc_ROCR, 2))) 


# gini
bank_logistic_gini<-(bank_logistic_auc_ROCR*2)-1
bank_logistic_gini #0.2658055

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}


## As lift / gain chart is for predicting the defaulters,
## Hence defaulters are choosen as 1 (instead of 0) and predicted values also changed
#actual_resp_logistic_bank <- ifelse(test_bank_logistic$Pelogisticormance.Tag == 0,1,0)
#summary(factor(actual_resp_logistic_bank))
#bank_data_logistic_pred_val <- 1-bank_data_logistic_pred
actual_resp_logistic_bank <- ifelse(factor(test_bank$Performance.Tag)== 0,1,0)
summary(factor(actual_resp_logistic_bank))
bank_data_logistic_pred_val <- 1-test_bank_logistic_pred



bank_logistic_lift_decile_info = lift(actual_resp_logistic_bank,bank_data_logistic_pred_val , groups = 10)

summary(factor(actual_response_logistic_bank))
summary(factor(predicted_response_bank_logistic))
print(bank_logistic_lift_decile_info)
write.csv(bank_logistic_lift_decile_info, "lift_bank_logistic.csv", row.names = FALSE)


###Plotting Gain Chart
ggplot(bank_logistic_lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=bank_logistic_lift_decile_info,aes(x=bucket,y=Gain),color='Green', group = 1,size=2,shape=20,stroke=2.5)+
  geom_line(data=bank_logistic_lift_decile_info,aes(x=bucket,y=Gain),color='Blue',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 100, 20),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart for Logistic Regression")

###Plotting Lift Chart
ggplot(bank_logistic_lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=bank_logistic_lift_decile_info,aes(x=bucket,y=Cumlift),color='Green', group = 1,size=2,shape=20,stroke=2.5)+
  geom_line(data=bank_logistic_lift_decile_info,aes(x=bucket,y=Cumlift),color='Blue',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(1, 1.8, 0.2))+
  ggtitle("Lift Chart for Logistic Regression")



#____________________________________________________________________________________________________#
######################################################################################################
############################ MODEL 2: Random Forest: (for entire Bank Data) ##########################
######################################################################################################
#____________________________________________________________________________________________________#

train_bank_rf_smote <-train_bank_smoted_bkp
test_bank_rf<-test_bank_bkp
#str(train_bank_rf_smote$Performance.Tag)
#str(test_bank_rf$Performance.Tag)
#test_bank_rf$Performance.Tag <-factor(test_bank_rf$Performance.Tag)
#train_bank_rf_smote$Performance.Tag <-factor(train_bank_rf_smote$Performance.Tag)

set.seed(108)

## Creatin initial random forest without tuning
bank_data_rf_init <- randomForest(Performance.Tag ~ ., data=train_bank_rf_smote, proximity=FALSE,
                                  do.trace=TRUE, na.action=na.omit);
## Evalution
bank_data_rf_init
## OOB estimate of  error rate: 11.24%
## Error on choosing Default cass 22.78% & Error on choosing non-defaulters 0.97%

plot(bank_data_rf_init)
# Error exponentially decreasing when "no of trees"increases. When tree size is 300, error is nearly constant. 
# Hence ntree is choosen as 300


## optimizing mtry with ntree =300
tuning_forest<-tuneRF(train_bank_rf_smote[,-2] , train_bank_rf_smote[,2],
                      stepFactor = 1.2,
                      plot = TRUE,
                      ntreeTry = 300, trace = TRUE,
                      improve = 0.001)
## When mtry parameter is 7, OOB error is minimum
set.seed(108)
## building random forest model with optimized  mtry and ntree parameter
bank_data_rf_model <- randomForest(Performance.Tag ~ ., data=train_bank_rf_smote, proximity=FALSE,
                                   do.trace=TRUE, na.action=na.omit,ntree = 300, mtry = 7);
bank_data_rf_model


final_bank_data_rf <- bank_data_rf_model
# predicting based on "Random Forest" model
bank_data_rf_pred <- predict(final_bank_data_rf, test_bank_rf, type = "prob")[,2]

summary(bank_data_rf_pred)
## Min value 0.3267, Max Value  0.9967 

s = seq( 0.3267,0.9967,length=100)
OUT_BANK_RF = matrix(0,100,3)

############## Below function predict "sensitivity", "specificity", "accuracy" for a certain cutoff value
## optimal cutoff identification
perform_fn <- function(cutoff,pred_data,actual_response) 
{
  predicted_response<-as.factor(ifelse(pred_data >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, actual_response, positive = "0")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

########################################################################
actual_response_rf_bank<-as.factor(test_bank_rf$Performance.Tag)
summary(actual_response_rf_bank)
# calculate the sens, spec and acc for different cutoff values
for(i in 1:100)
{
  OUT_BANK_RF[i,] = perform_fn(s[i],bank_data_rf_pred,actual_response_rf_bank)
} 
OUT_BANK_RF

## Plotting Cutoff Value
plot(s, OUT_BANK_RF[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_BANK_RF[,2],col="darkgreen",lwd=2)
lines(s,OUT_BANK_RF[,3],col="blue",lwd=2)
abline(v =0.17)
box()
legend(0.4,.50,col=c(2,"darkgreen","blue","darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))



## prediting CUTOFF value for model
cutoff_val <- min(abs(OUT_BANK_RF[,1]-OUT_BANK_RF[,2]))
cutoff_final_bank_rf <- s[which(abs(OUT_BANK_RF[,1]-OUT_BANK_RF[,2])==cutoff_val)]
cutoff_final_bank_rf



predicted_response_bank_rf <- factor(ifelse(bank_data_rf_pred >= cutoff_final_bank_rf, "1", "0"))
conf_matr_bank_rf <- confusionMatrix(predicted_response_bank_rf, factor(test_bank_rf$Performance.Tag), positive = "0")
conf_matr_bank_rf

## Accuracy = 62.4% / Sensitivity = 62.48 % / Specificity = 62.46 %
##Result for using model "bank_data_rf_model"

##################################################################################################

control <- trainControl(method="cv", number=5)
metric <- "Accuracy"
set.seed(111)
mtry <- (5:9)
tunegrid <- expand.grid(.mtry=mtry)

rf_default_model <- train(Performance.Tag~., data=train_bank_rf_smote, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default_model)
#Summary of sample sizes: 13790, 13790, 13790, 13791, 13791 
#Resampling results across tuning parameters:

#  mtry  Accuracy   Kappa    
#  5     0.8843250  0.7649780
#  6     0.8854853  0.7673335
#  7     0.8840350  0.7643528
#  8     0.8846151  0.7655287
#  9     0.8839769  0.7642202

plot(rf_default_model)

evaluate_model_bank_rf_pred <- predict(rf_default_model, test_bank_rf, type = "prob")[,2]
summary(evaluate_model_bank_rf_pred)
## Min value 0.3520, Max Value  0.9980 
s_mod = seq( 0.3520,0.9980 ,length=100)
OUT_BANK_RF_mod = matrix(0,100,3)

actual_response_rf_bank_mod<-as.factor(test_bank_rf$Performance.Tag)
summary(actual_response_rf_bank_mod)
# calculate the sens, spec and acc for different cutoff values
for(i in 1:100)
{
  OUT_BANK_RF_mod[i,] = perform_fn(s_mod[i],evaluate_model_bank_rf_pred,actual_response_rf_bank_mod)
} 
OUT_BANK_RF_mod


## prediting CUTOFF value for model
cutoff_val_mod <- min(abs(OUT_BANK_RF_mod[,1]-OUT_BANK_RF_mod[,2]))
cutoff_final_bank_rf_mod <- s_mod[which(abs(OUT_BANK_RF_mod[,1]-OUT_BANK_RF_mod[,2])==cutoff_val_mod)]
cutoff_final_bank_rf_mod

predicted_response_bank_rf_mod <- factor(ifelse(evaluate_model_bank_rf_pred >= cutoff_final_bank_rf_mod, "1", "0"))
conf_forest_mer_mod <- confusionMatrix(predicted_response_bank_rf_mod, factor(test_bank_rf$Performance.Tag), positive = "0")
conf_forest_mer_mod
## Accuracy = 61.7% / Sensitivity = 61.6 % / Specificity = 61.7 %
##Result for using model "rf_default_model"


## Comparing two random forest model, first one is choosen as it provides better result
## Hence "bank_data_rf_model" choosen

##################################################################################################
### KS -statistic - Test Data ######
summary(predicted_response_bank_rf)
summary(actual_response_rf_bank)
pred_object_bank_test_rf<- prediction(as.numeric(predicted_response_bank_rf), as.numeric(actual_response_rf_bank))
bank_rf_performance_measures_test<- performance(pred_object_bank_test_rf, "tpr", "fpr")

bank_rf_ks_table_test <- attr(bank_rf_performance_measures_test, "y.values")[[1]] - 
  (attr(bank_rf_performance_measures_test, "x.values")[[1]])
max(bank_rf_ks_table_test) # 0.2495206

#KS-statistic for Bank Data (using Random Forest) is 24% 

#ROC Curve

bank_rf_auc_ROCR <- performance(pred_object_bank_test_rf, measure = "auc")
bank_rf_auc_ROCR <- bank_rf_auc_ROCR@y.values[[1]]
bank_rf_auc_ROCR 
## # Area under curve for Bank Data (using Random Forest) is : 0.6247603

pd_bank_rf <- data.frame(fpr=unlist(bank_rf_performance_measures_test@x.values), tpr=unlist(bank_rf_performance_measures_test@y.values))

## ROC Curve for Random Forest
ggplot(pd_bank_rf ,aes(x=fpr, y=tpr)) +
  geom_line(colour="blue") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Random Forest") +
  annotate("text", x=0.4, y=0.00, size=5,label=paste("AUC  =", round(bank_rf_auc_ROCR, 2))) 


# gini
bank_rf_gini<-(bank_rf_auc_ROCR*2)-1
bank_rf_gini #0.2495206




lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

## As lift / gain chart is for predicting the defaulters,
## Hence defaulters are choosen as 1 (instead of 0) and predicted values also changed
actual_resp_rf_bank <- ifelse(test_bank_rf$Performance.Tag == 0,1,0)
summary(factor(actual_resp_rf_bank))
bank_data_rf_pred_val <- 1-bank_data_rf_pred

bank_rf_lift_decile_info = lift(actual_resp_rf_bank,bank_data_rf_pred_val , groups = 10)

summary(actual_response_rf_bank)
summary(predicted_response_bank_rf)
print(bank_rf_lift_decile_info)
write.csv(bank_rf_lift_decile_info, "lift_bank_rf.csv", row.names = FALSE)



###Plotting Gain Chart
ggplot(bank_rf_lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=bank_rf_lift_decile_info,aes(x=bucket,y=Gain),color='Green', group = 1,size=2,shape=20,stroke=2.5)+
  geom_line(data=bank_rf_lift_decile_info,aes(x=bucket,y=Gain),color='Blue',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 100, 20),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart for Random Forest")


###Plotting Lift Chart
ggplot(bank_rf_lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=bank_rf_lift_decile_info,aes(x=bucket,y=Cumlift),color='Green', group = 1,size=2,shape=20,stroke=2.5)+
  geom_line(data=bank_rf_lift_decile_info,aes(x=bucket,y=Cumlift),color='Blue',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(1, 1.8, 0.2))+
  ggtitle("Lift Chart for Random Forest")




######################################################################################################
######################### Model building and evaluation for Demographic Data #########################
######################################################################################################

# Analysis on Demographic Data
str(Bank_Data_woe)

Demographic_Columns<-c("Age","Gender","Marital.Status..at.the.time.of.application.","No.of.dependents","Income","Education","Profession",
                       "Type.of.residence","No.of.months.in.current.residence","No.of.months.in.current.company","Performance.Tag")

Demographic_Data_woe <- Bank_Data_woe[,Demographic_Columns]
str(Demographic_Data_woe)

######################################################################################################
######################### Model building and evaluation for Entire Demographic Data #########################
######################################################################################################

# splitting the data between train and test
set.seed(321)

indices = sample.split(Demographic_Data_woe$Performance.Tag, SplitRatio = 0.7)

train_demographic = Demographic_Data_woe[indices,]
train_demographic_bkp = train_demographic
test_demographic = Demographic_Data_woe[!(indices),]
test_demographic_bkp = test_demographic

##########################################################################
#_______________Derived Trained Data as number of defaulers is very less______________
summary(factor(train_demographic$Performance.Tag))

## So number of defaulters is very less around 4.2% of data, hence the data is very unbalanced and biased to "Good Customers"
train_demographic$Performance.Tag<-factor(train_demographic$Performance.Tag)
train_demographic_smoted <- SMOTE("Performance.Tag"~., train_demographic, perc.over=300,perc.under=150, seed = 321)
#train_demographic_smoted <- SMOTE("Performance.Tag"~., train_demographic, perc.over=100,perc.under=200)
train_demographic_smoted_bkp = train_demographic_smoted
summary(train_demographic_smoted$Performance.Tag)
# Number of good Customers: 9126 , Number of Defaulters = 8112 
# Now number of defaultes (Represents by Performance.Tag= 0) is significant (47%)


#____________________________________________________________________________________________________#
######################################################################################################
################### MODEL 3: Logistic Regression: (for entire Demographic Data) ######################
######################################################################################################
#____________________________________________________________________________________________________#
set.seed(321)
### Model Evaluation
logistic_demographic_model_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_demographic_smoted)
summary(logistic_demographic_model_1)

# many columns found insignificant
# Using stepwise algorithm for removing insignificant variables 
logistic_demographic_model_2 <- stepAIC(logistic_demographic_model_1, direction="both")
summary(logistic_demographic_model_2)
vif(logistic_demographic_model_2)


# removing "Education"
logistic_demographic_model_3 <- glm(formula = Performance.Tag ~ 
                                      Age + 
                                      Gender + 
                                      No.of.dependents + 
                                      Income + 
                                      Profession + 
                                      Type.of.residence + 
                                      No.of.months.in.current.residence + 
                                      No.of.months.in.current.company
                                    , family = "binomial", data = train_demographic_smoted)
summary(logistic_demographic_model_3)
vif(logistic_demographic_model_3)

# removing "Profession"
logistic_demographic_model_4 <- glm(formula = Performance.Tag ~ 
                                      Age + 
                                      Gender + 
                                      No.of.dependents + 
                                      Income +  
                                      Type.of.residence + 
                                      No.of.months.in.current.residence + 
                                      No.of.months.in.current.company
                                    , family = "binomial", data = train_demographic_smoted)
summary(logistic_demographic_model_4)
vif(logistic_demographic_model_4)


# removing "Type.of.residence "
logistic_demographic_model_5 <- glm(formula = Performance.Tag ~ 
                                      Age + 
                                      Gender + 
                                      No.of.dependents + 
                                      Income +  
                                      No.of.months.in.current.residence + 
                                      No.of.months.in.current.company
                                    , family = "binomial", data = train_demographic_smoted)
summary(logistic_demographic_model_5)
vif(logistic_demographic_model_5)

# removing "Gender"
logistic_demographic_model_6 <- glm(formula = Performance.Tag ~ 
                                      Age + 
                                      No.of.dependents + 
                                      Income +  
                                      No.of.months.in.current.residence + 
                                      No.of.months.in.current.company
                                    , family = "binomial", data = train_demographic_smoted)
summary(logistic_demographic_model_6)
vif(logistic_demographic_model_6)

final_model_demographic_logistic=logistic_demographic_model_6

########################################################################
########  Logistic Model Validation for entire Demographic Data ########
########################################################################

test_demographic_logistic_pred = predict(final_model_demographic_logistic, type = "response", 
                                         newdata = test_demographic[,-11])  #second column is performance tag

# Summary of prediction
summary(test_demographic_logistic_pred)


test_demographic$prob <- test_demographic_logistic_pred
#View(test_demographic)


#____________________________________________
###Model Evalution (assuming cutoff is 0.50)
## Checking model assuming cutoff is 0.50 (i.e. if "prediction value" <= 0.50 then Performance.Tag= 0)
test_demographic_pred_tag <- factor(ifelse(test_demographic_logistic_pred <= 0.50, "Yes", "No"))
test_demographic_actual_tag <- factor(ifelse(test_demographic$Performance.Tag==0,"Yes","No"))
table(test_demographic_actual_tag,test_demographic_pred_tag)
test_demographic_conf <- confusionMatrix(test_demographic_pred_tag, test_demographic_actual_tag, positive = "Yes")
test_demographic_conf
#Accuracy 67.7% , Sensitivity 43% , Specificity is nearly 68.8%
#____________________________________________


#____________________________________________
## Cut Off Value calculation
#____________________________________________
summary(test_demographic_logistic_pred)
# Creating cutoff values from 0.2669  (Min Value) to 0.7751  (Max Value) for plotting and initiallizing a matrix of 100 X 3.
# Summary of test probability
s = seq(0.2669 ,0.7751,length=100)

############## Below function predict "sensitivity", "specificity", "accuracy" for a certain cutoff value
## optimal cutoff identification
perform_prob_cutoff_fn <- function(cutoff,test_actual,test_pred) 
{
  predicted_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

OUT_DEMOGRAPHIC_LOGISTIC = matrix(0,100,3)

test_demographic_actual_tag_logistic <- factor(ifelse(test_demographic$Performance.Tag==0,"Yes","No"))
for(i in 1:100)
{
  OUT_DEMOGRAPHIC_LOGISTIC[i,] = perform_prob_cutoff_fn(s[i],test_demographic_actual_tag_logistic,test_demographic$prob)
} 
OUT_DEMOGRAPHIC_LOGISTIC

cutoff_val <- min(abs(OUT_DEMOGRAPHIC_LOGISTIC[,1]-OUT_DEMOGRAPHIC_LOGISTIC[,2]))


plot(s, OUT_DEMOGRAPHIC_LOGISTIC[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_DEMOGRAPHIC_LOGISTIC[,2],col="darkgreen",lwd=2)
lines(s,OUT_DEMOGRAPHIC_LOGISTIC[,3],col="blue",lwd=2)
box()
legend(.6,1,col=c(2,"darkgreen","blue","darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_final <- s[which(abs(OUT_DEMOGRAPHIC_LOGISTIC[,1]-OUT_DEMOGRAPHIC_LOGISTIC[,2])==cutoff_val)]
cutoff_final

# predicting test record using derived cutoff value on  final model
test_demographic_pred_tag_logistic <- factor(ifelse(test_demographic$prob <=cutoff_final, "Yes","No"))
table(test_demographic_actual_tag_logistic,test_demographic_pred_tag_logistic)
conf_final <- confusionMatrix(test_demographic_pred_tag_logistic, test_demographic_actual_tag_logistic, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

## Accuracy = 57.26% / Sensitivity = 54.89 % / Specificity = 57.36 %

##################################################################################################
### KS -statistic - Test Data ######
summary(test_demographic_pred_tag_logistic)
summary(test_demographic_actual_tag_logistic)
predicted_response_demographic_logistic <- ifelse(test_demographic_pred_tag_logistic=="Yes",1,0)
actual_response_logistic_demographic <- ifelse(test_demographic_actual_tag_logistic=="Yes",1,0)

pred_object_demographic_test_logistic<- prediction(as.numeric(predicted_response_demographic_logistic), as.numeric(actual_response_logistic_demographic))
demographic_logistic_performance_measures_test<- performance(pred_object_demographic_test_logistic, "tpr", "fpr")


demographic_logistic_ks_table_test <- attr(demographic_logistic_performance_measures_test, "y.values")[[1]] - 
  (attr(demographic_logistic_performance_measures_test, "x.values")[[1]])
max(demographic_logistic_ks_table_test) # 0.1225204

#KS-statistic for Demographic Data (using Logistic Regression) is 12%

#ROC Curve

demographic_logistic_auc_ROCR <- performance(pred_object_demographic_test_logistic, measure = "auc")
demographic_logistic_auc_ROCR <- demographic_logistic_auc_ROCR@y.values[[1]]
demographic_logistic_auc_ROCR 
## # Area under curve for Demographic Data (using Logistic Regression) is : 0.5612602


pd_demographic_logistic <- data.frame(fpr=unlist(demographic_logistic_performance_measures_test@x.values), tpr=unlist(demographic_logistic_performance_measures_test@y.values))

## ROC Curve for Logistic Regression
ggplot(pd_demographic_logistic ,aes(x=fpr, y=tpr)) +
  geom_line(colour="blue") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Logistic Regression") +
  annotate("text", x=0.4, y=0.00, size=5,label=paste("AUC  =", round(demographic_logistic_auc_ROCR, 2))) 



# gini
demographic_logistic_gini<-(demographic_logistic_auc_ROCR*2)-1
demographic_logistic_gini #0.1225204


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

## As lift / gain chart is for predicting the defaulters,
## Hence defaulters are choosen as 1 (instead of 0) and predicted values also changed
#actual_resp_logistic_demographic <- ifelse(test_demographic_logistic$Pelogisticormance.Tag == 0,1,0)
#summary(factor(actual_resp_logistic_demographic))
#demographic_data_logistic_pred_val <- 1-demographic_data_logistic_pred
actual_resp_logistic_demographic <- ifelse(factor(test_demographic$Performance.Tag)== 0,1,0)
summary(factor(actual_resp_logistic_demographic))
demographic_data_logistic_pred_val <- 1-test_demographic_logistic_pred



demographic_logistic_lift_decile_info = lift(actual_resp_logistic_demographic,demographic_data_logistic_pred_val , groups = 10)

summary(factor(actual_response_logistic_demographic))
summary(factor(predicted_response_demographic_logistic))
print(demographic_logistic_lift_decile_info)
write.csv(demographic_logistic_lift_decile_info, "lift_demographic_logistic.csv", row.names = FALSE)


###Plotting Gain Chart
ggplot(demographic_logistic_lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=demographic_logistic_lift_decile_info,aes(x=bucket,y=Gain),color='Green', group = 1,size=2,shape=20,stroke=2.5)+
  geom_line(data=demographic_logistic_lift_decile_info,aes(x=bucket,y=Gain),color='Blue',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 100, 20),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart for Logistic Regression for demographic data")

###Plotting Lift Chart
ggplot(demographic_logistic_lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=demographic_logistic_lift_decile_info,aes(x=bucket,y=Cumlift),color='Green', group = 1,size=2,shape=20,stroke=2.5)+
  geom_line(data=demographic_logistic_lift_decile_info,aes(x=bucket,y=Cumlift),color='Blue',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(1, 1.8, 0.2))+
  ggtitle("Lift Chart for Logistic Regression for demographic data")


#____________________________________________________________________________________________________#
######################################################################################################
###################### MODEL 4: Random Forest: (for entire Demographic Data) #########################
######################################################################################################
#____________________________________________________________________________________________________#

train_demographic_rf_smote <-train_demographic_smoted_bkp
test_demographic_rf<-test_demographic_bkp
train_demographic_rf_smote$Performance.Tag <- factor(train_demographic_rf_smote$Performance.Tag)

str(train_demographic_rf_smote$Performance.Tag)
set.seed(108)

## Creatin initial random forest without tuning
demographic_data_rf_init <- randomForest(Performance.Tag ~ ., data=train_demographic_rf_smote, proximity=FALSE,
                                         do.trace=TRUE, na.action=na.omit);
## Evalution
demographic_data_rf_init
## OOB estimate of  error rate: 11.95%
## Error on choosing Default cass 23.20% & Error on choosing non-defaulters 1.95%

plot(demographic_data_rf_init)
# Error exponentially decreasing when "no of trees"increases. When tree size is 300, error is nearly constant. 
# Hence ntree is choosen as 300


## optimizing mtry with ntree =300
tuning_forest<-tuneRF(train_demographic_rf_smote[,-2] , train_demographic_rf_smote[,2],
                      stepFactor = 1.2,
                      plot = TRUE,
                      ntreeTry = 300, trace = TRUE,
                      improve = 0.001)
## When mtry parameter is 3, OOB error is minimum


set.seed(108)
## building random forest model with optimized  mtry and ntree parameter
demographic_data_rf_model <- randomForest(Performance.Tag ~ ., data=train_demographic_rf_smote, proximity=FALSE,
                                          do.trace=TRUE, na.action=na.omit,ntree = 300, mtry = 3);
demographic_data_rf_model


final_demographic_data_rf <- demographic_data_rf_model

# predicting based on "Random Forest" model
demographic_data_rf_pred <- predict(final_demographic_data_rf, test_demographic_rf, type = "prob")[,2]

summary(demographic_data_rf_pred)
## Min value 0.3267, Max Value  0.9967 

s = seq( 0.3267,0.9967,length=100)
OUT_DEMOGRAPHIC_RF = matrix(0,100,3)

############## Below function predict "sensitivity", "specificity", "accuracy" for a certain cutoff value
## optimal cutoff identification
perform_fn <- function(cutoff,pred_data,actual_response) 
{
  predicted_response<-as.factor(ifelse(pred_data >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, actual_response, positive = "0")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}




########################################################################
actual_response_rf_demographic<-as.factor(test_demographic_rf$Performance.Tag)
summary(actual_response_rf_demographic)
# calculate the sens, spec and acc for different cutoff values
for(i in 1:100)
{
  OUT_DEMOGRAPHIC_RF[i,] = perform_fn(s[i],demographic_data_rf_pred,actual_response_rf_demographic)
} 
OUT_DEMOGRAPHIC_RF

## Plotting Cutoff Value
plot(s, OUT_DEMOGRAPHIC_RF[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_DEMOGRAPHIC_RF[,2],col="darkgreen",lwd=2)
lines(s,OUT_DEMOGRAPHIC_RF[,3],col="blue",lwd=2)
abline(v =0.17)
box()
legend(0.4,.50,col=c(2,"darkgreen","blue","darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))



## prediting CUTOFF value for model
cutoff_val <- min(abs(OUT_DEMOGRAPHIC_RF[,1]-OUT_DEMOGRAPHIC_RF[,2]))
cutoff_final_demographic_rf <- s[which(abs(OUT_DEMOGRAPHIC_RF[,1]-OUT_DEMOGRAPHIC_RF[,2])==cutoff_val)]
cutoff_final_demographic_rf



predicted_response_demographic_rf <- factor(ifelse(demographic_data_rf_pred >= cutoff_final_demographic_rf, "1", "0"))
conf_matr_demographic_rf <- confusionMatrix(predicted_response_demographic_rf, factor(test_demographic_rf$Performance.Tag), positive = "0")
conf_matr_demographic_rf

## Accuracy = 52.99% / Sensitivity = 53.97 % / Specificity = 52.94 %
##Result for using model "demographic_data_rf_model"

##################################################################################################


##################################################################################################
### KS -statistic - Test Data ######
summary(predicted_response_demographic_rf)
summary(actual_response_rf_demographic)
pred_object_demographic_test_rf<- prediction(as.numeric(predicted_response_demographic_rf), as.numeric(actual_response_rf_demographic))
demographic_rf_performance_measures_test<- performance(pred_object_demographic_test_rf, "tpr", "fpr")

demographic_rf_ks_table_test <- attr(demographic_rf_performance_measures_test, "y.values")[[1]] - 
  (attr(demographic_rf_performance_measures_test, "x.values")[[1]])
max(demographic_rf_ks_table_test) # 0.0691513

#KS-statistic for Demographic Data (using Random Forest) is 6.9%

#ROC Curve

demographic_rf_auc_ROCR <- performance(pred_object_demographic_test_rf, measure = "auc")
demographic_rf_auc_ROCR <- demographic_rf_auc_ROCR@y.values[[1]]
demographic_rf_auc_ROCR 
## # Area under curve for Demographic Data (using Random Forest) is : 0.5345756


pd_demographic_rf <- data.frame(fpr=unlist(demographic_rf_performance_measures_test@x.values), tpr=unlist(demographic_rf_performance_measures_test@y.values))

## ROC Curve for Random Forest
ggplot(pd_demographic_rf ,aes(x=fpr, y=tpr)) +
  geom_line(colour="blue") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Random Forest") +
  annotate("text", x=0.4, y=0.00, size=5,label=paste("AUC  =", round(demographic_rf_auc_ROCR, 2))) 


# gini
demographic_rf_gini<-(demographic_rf_auc_ROCR*2)-1
demographic_rf_gini #0.0691513


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}



## As lift / gain chart is for predicting the defaulters,
## Hence defaulters are choosen as 1 (instead of 0) and predicted values also changed
actual_resp_rf_demographic <- ifelse(test_demographic_rf$Performance.Tag == 0,1,0)
summary(factor(actual_resp_rf_demographic))
demographic_data_rf_pred_val <- 1-demographic_data_rf_pred

demographic_rf_lift_decile_info = lift(actual_resp_rf_demographic,demographic_data_rf_pred_val , groups = 10)

summary(actual_response_rf_demographic)
summary(predicted_response_demographic_rf)
print(demographic_rf_lift_decile_info)
write.csv(demographic_rf_lift_decile_info, "lift_demographic_rf.csv", row.names = FALSE)



###Plotting Gain Chart
ggplot(demographic_rf_lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=demographic_rf_lift_decile_info,aes(x=bucket,y=Gain),color='Green', group = 1,size=2,shape=20,stroke=2.5)+
  geom_line(data=demographic_rf_lift_decile_info,aes(x=bucket,y=Gain),color='Blue',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 100, 20),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart for Random Forest for Demographic data")


###Plotting Lift Chart
ggplot(demographic_rf_lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=demographic_rf_lift_decile_info,aes(x=bucket,y=Cumlift),color='Green', group = 1,size=2,shape=20,stroke=2.5)+
  geom_line(data=demographic_rf_lift_decile_info,aes(x=bucket,y=Cumlift),color='Blue',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(1, 1.8, 0.2))+
  ggtitle("Lift Chart for Random Forest for Demographic data")

################################################
# XG Boost
################################################
#---------------------------------------------------------    

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(Bank_Data_woe$Performance.Tag, SplitRatio = 0.70)

train_xg <- Bank_Data_woe[split_indices, ]

test_xg <- Bank_Data_woe[!split_indices, ]

nrow(train_xg)/nrow(Bank_Data_woe)

nrow(test_xg)/nrow(Bank_Data_woe)



#### Applying xgboost

outcome <- train_xg$Performance.Tag;

train_matrix = data.matrix(train_xg[,-c(2)]);

test_matrix <- data.matrix(test_xg[,-c(2)]);

nrow(test_xg); #20614

zeros <- rep(0, 20614)

control <- 65;


##Applying the xgboost for 65 times and then taking the average probabilities for each  iteration

for (i in 1:control){
  
  bst <- xgboost(data = train_matrix,
                 label = as.numeric(as.character(outcome)),
                 eta = 0.1,
                 max_depth = 6,
                 subsample = 0.8,
                 colsample_bytree = 0.8,
                 nrounds = 80,
                 objective = "binary:logistic",
                 eval_metric = "auc"
  );
  yhat <- predict(bst,test_matrix, type = "response")
  zeros <- zeros + yhat			 
  
}

zeros <- zeros/control;

## Finding the optimal cutoff point for the probability

test_xg$test_pred <- zeros;
test_actual_tag_xg <- factor(ifelse(test_xg$Performance.Tag == 1, "Yes", "No"));

perform_fn_xg <- function(cutoff) 
{
  predicted_default_xg <- factor(ifelse(zeros >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default_xg, test_actual_tag_xg, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.62 to 0.99 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability


s = seq(0.05,0.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn_xg(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]
cutoff 

predicted_final <- factor(ifelse(zeros >= cutoff, "Yes", "No"));

conf_final <- confusionMatrix(predicted_final, test_actual_tag_xg, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

# Using XG Boost: Accuracy = 64.90%, Sensitivity = 65.08%, Specificity = 60.64%
# Applying XG Boost improves accuracy

#____________________________________________________________________________________________________#
######################################################################################################
######################################### Application scorecard ######################################
######################################################################################################
#____________________________________________________________________________________________________#

# based on performance, logistic model for entire data (model name "final_model_bank_logistic") is choosen 
str(Bank_Data_woe)
cutoff_Value<-cutoff_final_logistic

## Calculation for ScoreCard ##
##___________________________##
#Score = Offset + Factor ??? ln (odds)
#Score + pdo (i.e. " points to double the odds") = Offset + Factor ??? log (2 ??? odds)
# Deriving same,
#Factor = pdo / log (2);
#Offset = Score - {Factor ??? ln (Odds)}
# Score = Offset + {Factor ??? ln (Odds)}
#As this an application scorecard should have the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points. 
Offset= 400
pdo  = 20
#good to bad odds = 10 to 1

Factor = pdo / log (2) #28.8539

CUTOFF_SCORE <- Offset + Factor * (log((1 - cutoff_Value) / cutoff_Value) -log(10))
CUTOFF_SCORE
##___________________________##
##  CUTOFF value for Application Scorecard = 334.8953
##___________________________##


summary(factor(Bank_Data_woe$Performance.Tag))
Bank_pred = predict(final_model_bank_logistic, type = "response", 
                    newdata = Bank_Data_woe[,-2])  #second column is performance tag
summary(Bank_pred)
Pred_for_good<-sapply(Bank_pred,function(x) (1-x))
summary(Pred_for_good)
Odds_for_good<-sapply(Pred_for_good,function(x) (1-x)/x)
ln_Odds_for_good<-sapply(Odds_for_good,function(x)log(x))


application_score_card<-sapply(ln_Odds_for_good,function(x) 400 + Factor * (x - log(10)))
summary(application_score_card)
# min 310.9 to 372.7
boxplot(application_score_card,main = "approved applicants",horizontal = T)



score_card_df<-cbind(Bank_Data_woe,application_score_card)
#View(score_card_df)


summary(factor(score_card_df$Performance.Tag))
num_of_defaults_below_cutoff<-length(which(score_card_df$Performance.Tag==0 & score_card_df$application_score_card<=CUTOFF_SCORE))
num_of_defaults_below_cutoff
actual_num_of_defaults<- length(which(score_card_df$Performance.Tag==0))
actual_num_of_defaults

num_of_defaults_beyond_cutoff<-length(which(score_card_df$Performance.Tag==0 & score_card_df$application_score_card>CUTOFF_SCORE))
num_of_defaults_beyond_cutoff
# model failed to identify 923 defaulters only.

Defaulters_prediction_Percentage = num_of_defaults_below_cutoff / actual_num_of_defaults * 100
Defaulters_prediction_Percentage
## 68.14 % of total defaulters are predicted successfully as defaulters



num_of_non_defaults_below_cutoff<-length(which(score_card_df$Performance.Tag==1 & score_card_df$application_score_card<=CUTOFF_SCORE))
num_of_non_defaults_below_cutoff
actual_num_of_non_defaults<- length(which(score_card_df$Performance.Tag==1))
actual_num_of_non_defaults

num_of_non_defaults_below_cutoff / actual_num_of_non_defaults

num_of_non_defaults_beyond_cutoff<-length(which(score_card_df$Performance.Tag==1 & score_card_df$application_score_card>CUTOFF_SCORE))
num_of_non_defaults_beyond_cutoff




## Convert all data (including who not get credit card) in woe
str(Application_Bank_Data)
summary(factor(Application_Bank_Data$Performance.Tag))

Rejected_Application_No <- Application_Bank_Data$Application.ID[which(sapply(Application_Bank_Data$Performance.Tag,is.na))]
length(Rejected_Application_No)

# Swapping the defaultes and rejected applicants as 0 and good guys as 1
Application_Bank_Data$Performance.Tag <- ifelse(Application_Bank_Data$Performance.Tag == 0,1,0)
Application_Bank_Data$Performance.Tag <- replace(Application_Bank_Data$Performance.Tag, is.na(Application_Bank_Data$Performance.Tag), 0)


summary(factor(Application_Bank_Data$Performance.Tag))
IV_app <- create_infotables(data=Application_Bank_Data, 
                            y="Performance.Tag",
                            bins = 10,
                            parallel=FALSE)
IV_app_Value = data.frame(IV_app$Summary)

App_Bank_Data_woe <- woe_replace(Application_Bank_Data,IV_app,c("Application.ID"))
#View(App_Bank_Data_woe)

Rejected_BANK_Data_WOE <- App_Bank_Data_woe[App_Bank_Data_woe$Application.ID %in% Rejected_Application_No,]
#View(Rejected_BANK_Data_WOE)
#summary(factor(Rejected_BANK_Data_WOE$Performance.Tag))


# 
Rejected_App_Pred = predict(final_model_bank_logistic, type = "response", 
                            newdata = Rejected_BANK_Data_WOE[,-2])  #second column is performance tag
summary(Rejected_App_Pred)
Pred_for_good_in_Rejected<-sapply(Rejected_App_Pred,function(x) (1-x))
summary(Pred_for_good_in_Rejected)
Odds_for_good_in_Rejected<-sapply(Pred_for_good_in_Rejected,function(x) (1-x)/x)
ln_Odds_for_good_in_Rejected<-sapply(Odds_for_good_in_Rejected,function(x)log(x))


application_score_card_for_Rejected<-sapply(ln_Odds_for_good_in_Rejected,function(x) 400 + Factor * (x - log(10)))
summary(application_score_card_for_Rejected)
# min 298.6 to 347.3

boxplot(application_score_card_for_Rejected,main = "Rejected applicants",horizontal = T)


## Minimum And Median score of Rejected applicants is far less than Approved applicants
score_card_df_for_rejected<-cbind(Rejected_BANK_Data_WOE,application_score_card_for_Rejected)
#View(score_card_df)
num_of_defaulters_predicted<-length(which(score_card_df_for_rejected$application_score_card_for_Rejected<=CUTOFF_SCORE))
num_of_defaulters_predicted/nrow(Rejected_BANK_Data_WOE)*100
## 99.2 % of Rejected applicants are rejected by model as well

nrow(Rejected_BANK_Data_WOE)-num_of_defaulters_predicted
## Only 11 applicants are rejected previously by bank but approved by model







#____________________________________________________________________________________________________#
######################################################################################################
################################ Financial benefit of the model ######################################
######################################################################################################
#____________________________________________________________________________________________________#

## Assumption: (As no data is given, below assumtions are taken for calculating Financial benefit)
## average REVENUE LOSS of Rs.5000 when an non defaulters application is rejected
## average CREDIT LOSS of Rs. 100000 if a defaulters application is accepted

## Calculating Financial Benefits for Bank (Without using this Model)
summary(factor(Bank_Data$Performance.Tag))
No_of_non_defaulters <- 66789
No_of_defaulters <- 2945
Actual_REVENUE <- No_of_non_defaulters * 5000 - No_of_defaulters * 100000
Actual_REVENUE


## Calculating Financial Benefits for existing models by Bank
summary(factor(Bank_Data$Performance.Tag))
num_of_non_defaults_beyond_cutoff 
No_of_defaulters <- 2945
Predicted_REVENUE <- num_of_non_defaults_beyond_cutoff * 5000 - num_of_defaults_beyond_cutoff * 100000
Predicted_REVENUE


Financial_Benifits <- Predicted_REVENUE- Actual_REVENUE
Financial_Benifits # Rs  62265000



