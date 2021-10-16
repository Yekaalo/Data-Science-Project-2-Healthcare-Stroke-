library(ggplot2)
library(gridExtra)
library(ggridges)

data<- read.csv("healthcare-dataset-stroke-data.csv")

#summary of Data
summary(data)

# Convert into Categorical variables
data$gender<- factor(data$gender)
data$hypertension<- factor(data$hypertension)
data$heart_disease<- factor(data$heart_disease)
data$ever_married<- factor(data$ever_married)
data$work_type<- factor(data$work_type)
data$Residence_type<- factor(data$Residence_type)
data$smoking_status<- factor(data$smoking_status)
data$stroke<- factor(data$stroke)

#convert in Numberical variables
data$bmi<-as.numeric(data$bmi)

#Replacing numeric NA values with the mean
#Create a new dataset called data_fix
data_fix <- data
#Calculate the mean bmi and create a variable data_av_bmi
data_av_bmi <- mean(data$bmi,na.rm = TRUE)
#Set NA vaulues for bmi to the mea
data_fix$bmi[is.na(data_fix$bmi)]<-data_av_bmi

### Graph
#Data
ggplot(data, aes(x=bmi)) + geom_density()
#Data_Fix
ggplot(data_fix, aes(x=bmi)) + geom_density()


#Replacing categorical NA values with the mode
#Create a table to calculate the total of each category
ta_smoking_status <- table(data_fix$smoking_status)
max_smoking_status <- max(ta_smoking_status)
mod_smoking_status <- names(ta_smoking_status)[ta_smoking_status==max_smoking_status]
data_fix$smoking_status[is.na(data_fix$smoking_status)] <- mod_smoking_status

#Graph
ggp1 <- ggplot(data, aes(x=smoking_status)) + geom_bar() + coord_flip()
ggp2 <- ggplot(data_fix, aes(x=smoking_status)) + geom_bar() + coord_flip()
grid.arrange(ggp1,ggp2,ncol=2)


####Outlier Visuals
#Age
ggplot(data, aes(x=age)) + geom_histogram()

#BMI
ggplot(data, aes(x=bmi)) + geom_histogram()

#Average Glucose
ggplot(data, aes(x=avg_glucose_level)) + geom_histogram()


#### MIn-MAX Normalization
#Age
data$age_minmax<- (data$age - min(data$age))/(max(data$age) - min(data$age))
#Plot the result.  Make sure that the range of the x axis is between 0 and 1.
#Normalizaed Graph
ggplot(data, aes(age_minmax))+ 
  geom_density()
#Unnormalized Graph
ggplot(data, aes(age))+ 
  geom_density()

#Bmi
data_fix$bmi_minmax<-(data_fix$bmi - min(data_fix$bmi))/(max(data_fix$bmi) - min(data_fix$bmi))
#Plot the result.  Make sure that the range of the x axis is between 0 and 1.
#Normalizaed Graph
ggplot(data_fix, aes(bmi_minmax))+ 
  geom_density()
#Unnormalized Graph
ggplot(data_fix, aes(bmi))+ 
  geom_density()

#average glucose
data$avg_glucose_minmax<- (data$avg_glucose_level - min(data$avg_glucose_level))/(max(data$avg_glucose_level) - min(data$avg_glucose_level))
#Plot the result.  Make sure that the range of the x axis is between 0 and 1.
#Normalizaed Graph
ggplot(data, aes(avg_glucose_minmax))+ 
  geom_density()
#Unnormalized Graph
ggplot(data, aes(avg_glucose_level))+ 
  geom_density()


#### Graph Inverse square root and Natural Log 

#Normal Avg glucose level
ggplot(data, aes(avg_glucose_level))+ 
  geom_density()

#Natlog
data$avg_glu_natlog <- log(data$avg_glucose_level)
ggplot(data, aes(avg_glu_natlog))+ 
  geom_density()

# Inverse square root
data$avg_glu_invsqrt <- 1/sqrt(data$avg_glucose_level)
ggplot(data, aes(avg_glu_invsqrt))+ 
  geom_density()


#### Equal Binning BMI into a categorical variable with 3 bins
data$bmi_bin<- as.factor(cut(data$bmi,3, labels=FALSE))
#Visualize the resulting categorical variable with a bar plot
ggplot(data, aes(x=bmi_bin)) + 
  geom_bar()




###Visualizations

#Exploring Age Distribution by Stroke
ggplot(data, aes(x=age, color=stroke, fill=stroke)) +  
  geom_density(alpha=0.5) + 
  ggtitle("Age Distribution by Stroke")+
  scale_color_brewer(palette="Set2")



#Exploring relation between BMI, Avg Glucose and Stroke
ggplot(data, 
       aes(x = avg_glucose_level, 
           y = bmi_bin, 
           fill = stroke, alpha=0.5)) +
  geom_density_ridges() + 
  theme_ridges() +
  ggtitle("Observation of BMI, Average Glucose, and Stroke") +
  theme(legend.position = "none")

#Exploring Gender Distribution by Stroke
#Stacked bar chart
ggplot(data, aes(x=gender, fill=stroke)) + 
  geom_bar() +
  ggtitle("Gender Distribution by Stroke")+
  labs(fill="Gender") +
  xlab("gender") +
  ylab("Count")

#Exploring Smoking Status Distribution by Stroke 
#Side-by-side bar chart
ggplot(data, aes(x=smoking_status, fill=stroke)) + 
  geom_bar(position = "dodge") +
  labs(fill="Gender") +
  ggtitle("Smoking Status Distribution by Stroke")
  xlab("smoking_status") +
  ylab("Count")



