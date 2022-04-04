################################
##### ML Project - Part A  #####
################################
library(dplyr)

## Please put all the datasets in the same folder(Set As Working Directory)


#Uploading the dataset:
Heart <- read.csv('Xy_train.csv')

##################################
##### Working with NA Values #####
##################################

#make missing values as NA:
Heart$age <- ifelse(Heart$age > 100,NA,Heart$age) #age column:
Heart$ca <- ifelse(Heart$ca == 4,NA,Heart$ca) #ca column:
Heart$thal <- ifelse(Heart$thal == 0,NA,Heart$thal) #thal column:

#Check NA values:
library(Amelia)
missmap(Heart, col = c('yellow','black'),y.at = c(1),y.labels = c(''))

#Duplicated rows
which(duplicated(Heart) | duplicated(Heart[nrow(Heart):1, ])[nrow(Heart):1])

#Missing data in specific row:
row.missing <- function(Column){
  MissingValues <- c()
  for(i in 1:length(Column)){
    if(is.na(Column[i])){
      MissingValues <- c(MissingValues,i)
    }
  }
  print(MissingValues)
}
row.missing(Heart$thal)
row.missing(Heart$age)
row.missing(Heart$ca)

################################################
##### Preparing the Catergorical variables #####
################################################

#Change the categorical variables from int to factor (look at: str(Heart))

#Option 1: We won't use it !
#cols <- c("gender","cp","fbs","restecg","exang","slope","ca","thal")
#Heart[cols] <- lapply(Heart[cols], factor)

#Option 2: Use it :)
### Change factors names in columns:
Heart <- Heart  %>%
  mutate(
    gender = case_when(
      gender == 0 ~ "0: female",
      gender == 1 ~ "1: male"
    ),
    fbs = case_when(
      fbs == 0 ~ "0: fbs<=120",
      fbs == 1 ~ "1: fbs>120"
    ),
    exang = case_when(
      exang == 0 ~ "0: no",
      exang == 1 ~ "1: yes"
    ),
    cp = case_when(
      cp == 0 ~ "0: typical angina",
      cp == 1 ~ "1: atypical angina",
      cp == 2 ~ "2: non-anginal",
      cp == 3 ~ "3: asymptomatic"
    ),
    restecg = case_when(
      restecg == 0 ~ "0: normal",
      restecg == 1 ~ "1: ST-T wave abnormality",
      restecg == 2 ~ "2: hypertrophy"
    ),
    slope = case_when(
      slope == 0 ~ "0: upsloping",
      slope == 1 ~ "1: flat",
      slope == 2 ~ "2: downsloping"
    ),
    thal = case_when(
      thal == 1 ~ "1: fixed defect",
      thal == 2 ~ "2: normal",
      thal == 3 ~ "3: reversable defect"
    ),
    gender = as.factor(gender),
    fbs = as.factor(fbs),
    exang = as.factor(exang),
    cp = as.factor(cp),
    slope = as.factor(slope),
    ca = as.factor(ca),
    thal = as.factor(thal),
    restecg=as.factor(restecg)
  )

# The Target Variable:
Heart <- Heart %>% rename(Y = y) #change target name(To make it easy:
                                 #y is y axis,Y is the Target variable)

Heart$Y <- ifelse(Heart$Y == 1,0,1) #CHANGE Y 

Heart$Y <- as.factor(Heart$Y) #change Y to factor

Yprior<-length(which(Heart$Y==1))/length(Heart$Y) #Prior Probability 
min(Heart$age)
max(Heart$age)

########################################
########## Data Visualization ##########
########################################
library(ggplot2)

###############################
##### the Target variable #####
###############################

# Y label
ggplot(Heart,aes(Y)) + geom_bar(fill = '#3399FF') + #3399FF  #9933FF
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('The target variable') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5))


#################################
##### Categorical variables #####
#################################

#Gender
ggplot(Heart,aes(gender)) + geom_bar(aes(fill = Y))  + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('count Gender label') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.15, 0.9),legend.direction = "horizontal")

# cp
ggplot(Heart,aes(cp)) + geom_bar(aes(fill = Y)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('count Chest pain type label') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.847, 0.9),legend.direction = "horizontal")

# fbs
ggplot(Heart,aes(fbs)) + geom_bar(aes(fill = Y)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.05)) + #add counting
  ggtitle('count Fasting blood sugar label') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.847, 0.9),legend.direction = "horizontal")

# restecg
slices<-c(sum(Heart$restecg==0),sum(Heart$restecg==1),sum(Heart$restecg==2))
lbls=c("0-normal","1-having ST-T wave abnormality","2- left ventricular hyperthrophy")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie chart restECG") 

ggplot(Heart,aes(restecg)) + geom_bar(aes(fill = Y)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('count Resting electrocardiographic label') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.847, 0.9),legend.direction = "horizontal")

# exang
ggplot(Heart,aes(exang)) + geom_bar(aes(fill = Y)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('count Exercise induced angina label') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.7, 0.9),legend.direction = "horizontal")+
  scale_fill_manual(values=c("#345534", "#E69F00"))

# slope
ggplot(Heart,aes(slope)) + geom_bar(aes(fill = Y)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('count the Slope of the peak exerc ST segment label') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.15, 0.9),legend.direction = "horizontal")

# ca
ggplot(Heart,aes(ca)) + geom_bar(aes(fill = Y)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('count number of major Vessels label') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.847, 0.9),legend.direction = "horizontal")

# thal
ggplot(Heart,aes(thal)) + geom_bar(aes(fill = Y)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('count thal label') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.847, 0.9),legend.direction = "horizontal")


################################
##### Contiunous variables #####
################################

# age
ggplot(Heart,aes(x=age,fill = Y)) + geom_bar() + #Used
  ggtitle('variation of Age') +
  theme(legend.position = c(0.835, 0.9),legend.direction = "horizontal")

tapply(Heart$age, Heart$Y, summary) #Used.


# trestbps
ggplot(Heart,aes(x=trestbps,fill=Y)) + geom_histogram(binwidth = 6) +
  ggtitle('variation of resting blood pressure') +
  theme(legend.position = c(0.835, 0.9),legend.direction = "horizontal")

tapply(Heart$trestbps, Heart$Y, summary) 


# chol
qplot(Heart$chol,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for cholestorol", 
      xlab = "cholestorol",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(100,450))

ggplot(Heart,aes(x=chol,fill=Y)) + geom_histogram(binwidth = 5)+ #Used.
  ggtitle('variation of serum cholesterol') +
  theme(legend.position = c(0.835, 0.9),legend.direction = "horizontal")

tapply(Heart$chol, Heart$Y, summary) 


# thalach
ggplot(Heart,aes(x=thalach,fill=Y)) + geom_histogram(binwidth = 3) + #Used.
  ggtitle('variation of Maximum heart rate achieved') +
  theme(legend.position = c(0.165, 0.9),legend.direction = "horizontal")

tapply(Heart$thalach, Heart$Y, summary)


# oldpeak
ggplot(Heart,aes(x=oldpeak,fill=Y)) + geom_histogram(binwidth = 0.5) +  #Used.
  ggtitle('variation of ST depression include rest') +
  theme(legend.position = c(0.835, 0.9),legend.direction = "horizontal")

tapply(Heart$oldpeak, Heart$Y, summary)


####################################################
#####couples of attributes with high correlation###
#oldpeak and slope
ggplot(Heart,aes(x=oldpeak,fill=slope)) + geom_histogram(binwidth = 0.5) +  #Used.
  ggtitle('oldpeak and slope') +
  theme(legend.position = c(0.835, 0.9),legend.direction = "horizontal")
tapply(Heart$oldpeak, Heart$slope, summary)


# cp and exang
ggplot(Heart,aes(x=cp)) + geom_bar(aes(fill = exang)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('count Chest pain type and exang') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.847, 0.9),legend.direction = "horizontal")+
scale_fill_manual(values=c("#999999", "#E69F00"))


#thalach groups and exang

ggplot(Heart,aes(x=findInterval(Heart$thalach, c(100, 110, 120,130,140,150,160,170)))) + geom_bar(aes(fill = exang)) + #Used.
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) + #add counting
  ggtitle('max heart rate and exercise angina') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), #add percentage
            stat='count',position=position_fill(vjust=-5.5)) +
  theme(legend.position = c(0.5, 0.9),legend.direction = "horizontal")+
  scale_fill_manual(values=c("#999977", "#E69F14"))

####################################################

##### Comparing the Target variable in training and test set #####
Ytest <- read.csv('preds_sample.csv')
Ytest$y <- as.factor(Ytest$y)

Y_Train <- ggplot(Heart,aes(Y)) + geom_bar(fill = '#3399FF') +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) +
  ggtitle('Target variable from training set') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=-5.5))

y_Test <- ggplot(Ytest,aes(y)) + geom_bar(fill = '#9933FF') +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.1)) +
  ggtitle('Target variable from test set') +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=-3))

library("gridExtra")
grid.arrange(Y_Train,y_Test,nrow = 1)

####################################################


#######################
##### Correlation #####
#######################

library(corrplot)
library(polycor)
library(GGally)
library(corrr)



Heart2<-read.csv('Xy_train.csv')
M <- cor(na.omit(select(Heart2,c(age,trestbps,chol,thalach,oldpeak,cp,restecg,gender,fbs,exang,slope,ca,thal))))
corrplot.mixed(M, lower.col = "black",
               number.cex = 0,order="hclust",tl.cex = 1)

#or:
ggpairs(select(Heart,c(age,trestbps,chol,thalach,oldpeak,cp,restecg,gender,fbs,exang,slope,ca,thal)))

## Big Plots ##
hetcor(Heart)
ggpairs(Heart)


####correlations with Y
MM<-cor(Heart2,Heart2$Y,method="kendall")
corela<-MM[,15]



#####################################
##### Working with missing data #####
#####################################

#Dropping id column:
library(dplyr)
Heart <- select(Heart,-id)

#Dropping rows with NA columns:
Heart <- Heart %>% filter(!is.na(ca))
Heart <- Heart %>% filter(!is.na(thal))

# Fill NA values in age column:
ggplot(Heart,aes(ca,age)) + geom_boxplot()
ggplot(Heart,aes(x=age,fill=Y)) + geom_histogram(binwidth = 5) +
  facet_wrap(~ ca) + ggtitle('ca ~ age, fill by the Target variable') +
  theme(legend.position = c(0.5, 0.3))
tapply(Heart$age, Heart$ca, summary) 

ageMed <- as.data.frame(Heart %>% group_by(ca) %>% summarise(MED = median(age,na.rm = T)))
for(i in 1:length(Heart$age)){
  if(is.na(Heart[i,1])){#just for missing values
    Heart[i,1] <- ageMed[as.integer(Heart[i,12])+1,2] #change missing value by age-ca median.
  }
}


##########################
##### Discretization #####
##########################

# BloodPressureRange
BloodPressure <- function(tres){
  if(tres<120){
    tres <- 1
  }
  else if(tres<129){
    tres <- 2
  }
  else if(tres<139){
    tres <- 3
  }
  else{
    tres <- 4
  }
}
Heart$BloodPressureRange <- as.factor(sapply(Heart$trestbps,BloodPressure))
ggplot(Heart,aes(BloodPressureRange,fill = Y)) + geom_bar() +
  ggtitle('The BloodPressureType variable')+
  scale_fill_manual(values=c("#999900", "#E99954"))



# Thalach discrete
maxh <- function(tres){
  if(tres<=100){
    tres <- 1
  }
  else if(tres<=110){
    tres <- 2
  }
  else if(tres<=120){
    tres <- 3
  }
  else if(tres<130){
    tres <- 4
  }
  else if(tres<=140){
    tres <- 5
  }
  else if(tres<=150){
    tres <- 6
  }
  else if(tres<=160){
    tres <- 7
  }
  else if(tres<=170){
    tres <- 8
  }
  else{
    tres <- 9
  }
}
Heart$thalachDiscrete <- as.factor(sapply(Heart$thalach,maxh))
ggplot(Heart,aes(thalachDiscrete,fill = Y)) + geom_bar() +
  ggtitle('The thalach discrete variable')




