# manuscript title: "Experiment 2: Reliability of the Serial Reaction Time task: If at first you don't succeed, try try try again"
# authors: "Catia Oliveira, Marianna E. Hayiou-Thomas, Lisa Henderson, University of York"

library(e1071)
library(lme4)
library(dplyr)
library(tidyverse)
library(purrr)


# Read in file
Data <- read.csv("data/Study2_data_all_variables.csv", header = TRUE)

Data$logRT <- log(Data$RT)

# Select for reliability for last 600 trials
Data <- subset(Data, Epoch > 2)

#assign 0 or 1 to odd or even trials
Data$Even_Odd <- rep(c("0", "1"), length.out=nrow(Data))

Session1_Data <- subset(Data, Session == 1)
Session2_Data <- subset(Data, Session == 2)
Session3_Data <- subset(Data, Session == 3)

Session1_Prob <- subset(Session1_Data, Probability == 2)
Session1_Improb <- subset(Session1_Data, Probability == 1)
Session2_Prob <- subset(Session2_Data, Probability == 2)
Session2_Improb <- subset(Session2_Data, Probability == 1)
Session3_Prob <- subset(Session3_Data, Probability == 2)
Session3_Improb <- subset(Session3_Data, Probability == 1)

Data_results1 <- aggregate(RT ~ Participant, Session1_Data, mean)
Data_results2 <- aggregate(RT ~ Participant, Session2_Data, mean)
Data_results3 <- aggregate(RT ~ Participant, Session3_Data, mean)


# PARTICIPANT OUTLIER REMOVAL BASED ON ACCURACY

#--- Same group

#Accuracy
pptmeanacc <-aggregate(Accuracy ~ Participant, Session1_Data, sum)
pptmeanacc2 <-aggregate(Accuracy ~ Participant, Session2_Data, sum)
pptmeanacc3 <-aggregate(Accuracy ~ Participant, Session3_Data, sum)

#Z scores

pptmeanacc$AccZ <-scale(pptmeanacc$Accuracy, center = TRUE, scale = TRUE)
pptmeanacc2$AccZ2 <-scale(pptmeanacc2$Accuracy, center = TRUE, scale = TRUE)
pptmeanacc3$AccZ3 <-scale(pptmeanacc3$Accuracy, center = TRUE, scale = TRUE)


# PARTICIPANT OUTLIER REMOVAL BASED ON Response Times
#---- Same group

# Mean response times 
pptmean_RT1 <-aggregate(RT ~ Participant, data= Session1_Data, mean)
pptmean_RT2 <-aggregate(RT ~ Participant, data= Session2_Data, mean)
pptmean_RT3 <-aggregate(RT ~ Participant, data= Session3_Data, mean)

####z scores
pptmean_RT1$RTZ <-scale(pptmean_RT1$RT, center = TRUE, scale = TRUE)
pptmean_RT2$RTZ <-scale(pptmean_RT2$RT, center = TRUE, scale = TRUE)
pptmean_RT3$RTZ <-scale(pptmean_RT3$RT, center = TRUE, scale = TRUE)

# plot response times
hist(Session1_Data$RT)
hist(Session2_Data$RT)
hist(Session3_Data$RT)

#Removal participants

Session1_trim1 <- subset(Session1_Data, Participant != "P035")
Session3_trim1 <- subset(Session3_Data, Participant != "P009")

#rename datasets

Session1_finalData <- Session1_trim1
Session3_finalData <- Session3_trim1

# Combine datasets
Data.trimmed <- rbind(Session1_finalData, Session2_Data, Session3_finalData)

#..............................Descriptive statistics for correlations

Data.trimmed$Probability[Data.trimmed$Probability == 2] <- "Prob"
Data.trimmed$Probability[Data.trimmed$Probability == 1] <- "Improb"

#for split-half reliability
Desc <- Data.trimmed %>%
  group_by(Participant, Session, Probability, Even_Odd) %>%
  summarise(mean = mean(RT))

Wide <- Desc %>% group_by(Participant) %>% unite(Type, Probability, Session, sep = "_") %>% spread(Type, mean)

Wide <- mutate(Wide, Difference1 = Improb_1 - Prob_1,
               Difference2 = Improb_2 - Prob_2,
               Difference3 = Improb_3 - Prob_3,
               Ratio1.1 = (Improb_1 - Prob_1)/((Improb_1 + Prob_1)/2),
               Ratio2.1 = (Improb_2 - Prob_2)/((Improb_2 + Prob_2)/2),
               Ratio3.1 = (Improb_3 - Prob_3)/((Improb_3 + Prob_3)/2),
               Ratio1.2 = (Improb_1 - Prob_1)/Improb_1,
               Ratio2.2 = (Improb_2 - Prob_2)/Improb_2,
               Ratio3.2 = (Improb_3 - Prob_3)/Improb_3)

#combine difference scores in dataframe

Seq.Learning <- as.data.frame(list(Wide) %>% reduce(full_join, by = "Participant"))


#Divide dataframe into even and odd trials

Seq.Learning_odd <- subset(Seq.Learning, Even_Odd == 1)
Seq.Learning_even <- subset(Seq.Learning, Even_Odd == 0)

#Remove outliers for each dataframe - even and odd

#eliminate outlier datapoints function

remove_outliers <-  function(x) {
  mn <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  (x > mn + sd * 2.5) | (x < mn - sd * 2.5)
}

# Remove outliers Odd
trim_odd <- data.frame(Seq.Learning_odd, stringsAsFactors = FALSE)

#identify outlier datapoints
cols_split <- paste0(c("Difference1", "Difference2", "Difference3", "Ratio1.1", "Ratio2.1", "Ratio3.1", "Ratio1.2", "Ratio2.2", "Ratio3.2"))
out_odd <- sapply(trim_odd[cols_split], remove_outliers)
trim_odd[cols_split][out_odd] <- NA

Seq.Learning_odd <- type.convert(trim_odd)

# Remove outliers Even
trim_even <- data.frame(Seq.Learning_even, stringsAsFactors = FALSE)

#identify outlier datapoints
out_even <- sapply(trim_even[cols_split], remove_outliers)
trim_even[cols_split][out_even] <- NA

Seq.Learning_even <- type.convert(trim_even)


#Split-half reliability for regression slopes ----------------------------------------------------------------

Session1_Data_Odd <- subset(Session1_finalData, Even_Odd == 1)
Session1_Data_Even <- subset(Session1_finalData, Even_Odd == 0)
Session2_Data_Odd <- subset(Session2_Data, Even_Odd == 1)
Session2_Data_Even <- subset(Session2_Data, Even_Odd == 0)
Session3_Data_Odd <- subset(Session3_finalData, Even_Odd == 1)
Session3_Data_Even <- subset(Session3_finalData, Even_Odd == 0)

#session 1_odd
model1a <- lmer(logRT ~ Probability + (Probability|Participant),data= Session1_Data_Odd, REML=FALSE)

df <- coef(model1a)$Participant
df <- tibble::rownames_to_column(df, "Participant") 
df$Diff1_slopes <- df$Probability

# session 1_even
model1b <- lmer(logRT ~ Probability + (Probability|Participant),data= Session1_Data_Even, REML=FALSE)

df2 <- coef(model1b)$Participant
df2 <- tibble::rownames_to_column(df2, "Participant")
df2$Diff2_slopes <- df2$Probability

#session 2_odd
model2a <- lmer(logRT ~ Probability + (Probability|Participant),data= Session2_Data_Odd, REML=FALSE)

df3<- coef(model2a)$Participant
df3 <- tibble::rownames_to_column(df3, "Participant")
df3$Diff3_slopes <- df3$Probability  

#session 2_even

model2b <- lmer(logRT ~ Probability + (Probability|Participant),data= Session2_Data_Even, REML=FALSE)

df4 <- coef(model2b)$Participant
df4 <- tibble::rownames_to_column(df4, "Participant") 
df4$Diff4_slopes <- df4$Probability

# session 3_odd
model3a <- lmer(logRT ~ Probability + (Probability|Participant),data= Session3_Data_Odd , REML=FALSE)

df5 <- coef(model3a)$Participant
df5 <- tibble::rownames_to_column(df5, "Participant")
df5$Diff5_slopes <- df5$Probability

#session 3_even
model3b <- lmer(logRT ~ Probability + (Probability|Participant),data= Session3_Data_Even, REML=FALSE)

df6 <- coef(model3b)$Participant
df6 <- tibble::rownames_to_column(df6, "Participant")
df6$Diff6_slopes <- df6$Probability  

Slope_data <- as.data.frame(list(df,df2,df3,df4, df5, df6) %>% reduce(full_join, by = "Participant"))

Slope_data <- Slope_data[,c("Participant", "Diff1_slopes", "Diff2_slopes", "Diff3_slopes", "Diff4_slopes", "Diff5_slopes", "Diff6_slopes")]

#Remove outliers

# Remove outliers

Slope <- data.frame(Slope_data, stringsAsFactors = FALSE)

cols_slope <- paste0(c("Diff1_slopes", "Diff2_slopes", "Diff3_slopes", "Diff4_slopes", "Diff5_slopes", "Diff6_slopes"))
out_slope <- sapply(Slope[cols_slope],remove_outliers)

Slope[cols_slope][out_slope] <- NA

Slope_trim <- type.convert(Slope)


# Difference scores --------------------------------

D1 <- cor.test(Seq.Learning_even$Difference1, Seq.Learning_odd$Difference1)
D2 <- cor.test(Seq.Learning_even$Difference2, Seq.Learning_odd$Difference2)
D3 <- cor.test(Seq.Learning_even$Difference3, Seq.Learning_odd$Difference3)

# Ratio scores --------------------------------------------

R1.1 <- cor.test(Seq.Learning_even$Ratio1.1, Seq.Learning_odd$Ratio1.1)
R2.1 <- cor.test(Seq.Learning_even$Ratio2.1, Seq.Learning_odd$Ratio2.1)
R3.1 <- cor.test(Seq.Learning_even$Ratio3.1, Seq.Learning_odd$Ratio3.1)

R1.2 <- cor.test(Seq.Learning_even$Ratio1.2, Seq.Learning_odd$Ratio1.2)
R2.2 <- cor.test(Seq.Learning_even$Ratio2.2, Seq.Learning_odd$Ratio2.2)
R3.2 <- cor.test(Seq.Learning_even$Ratio3.2, Seq.Learning_odd$Ratio3.2)

#Regression slopes

RS1 <- cor.test(Slope_trim$Diff1_slopes, Slope_trim$Diff2_slopes, use = "complete.obs")
RS2 <- cor.test(Slope_trim$Diff3_slopes, Slope_trim$Diff4_slopes, use = "complete.obs")
RS3 <- cor.test(Slope_trim$Diff5_slopes, Slope_trim$Diff6_slopes, use = "complete.obs")


Split_half <- data.frame(Measure = c("Difference", "Ratio1", "Ratio2", "Regression slopes"), Split.half_1 = round(c(D1$estimate, R1.1$estimate, R1.2$estimate, RS1$estimate),3), Split.half_2 = round(c(D2$estimate, R2.1$estimate, R2.2$estimate, RS2$estimate),3), Split.half_3 = round(c(D3$estimate, R3.1$estimate, R3.2$estimate, RS3$estimate),3)) 

Split_half

