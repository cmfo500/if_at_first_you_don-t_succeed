# manuscript title: "Experiment 1: Reliability of the Serial Reaction Time task: If at first you don't succeed, try try try again"
# authors: "Catia Oliveira, Marianna E. Hayiou-Thomas, Lisa Henderson, University of York"


library(e107)
library(lme4)
library(tidyverse)

# Read in file
Data <- read.csv("data/Data_final_RT.csv", header = TRUE)

Data$Even_Odd <- rep(c("0", "1"), length.out=nrow(Data))


#Data <- subset(Data, Epoch > 2)
Data$logRT <- log(Data$RT)

Session1_Data <- subset(Data, Session == 1)
Session2_Data <- subset(Data, Session == 2)

Data_results1 <- aggregate(RT ~Participant, Session1_Data, mean)
Data_results2 <- aggregate(RT ~Participant, Session2_Data, mean)


# PARTICIPANT OUTLIER REMOVAL BASED ON ACCURACY

#--- Same group

#Accuracy
pptmeanacc <-aggregate(Accuracy ~ Participant, Session1_Data, sum)
pptmeanacc2 <-aggregate(Accuracy ~ Participant, Session2_Data, sum)

#Z scores

pptmeanacc$AccZ <-scale(pptmeanacc$Acc, center = TRUE, scale = TRUE)
pptmeanacc2$AccZ <-scale(pptmeanacc2$Acc, center = TRUE, scale = TRUE)



# PARTICIPANT OUTLIER REMOVAL BASED ON Response Times
#---- Same group

# Mean response times 
pptmean_RT1 <-aggregate(RT ~ Participant, data= Session1_Data, mean)
pptmean_RT2 <-aggregate(RT ~ Participant, data= Session2_Data, mean)

####z scores
pptmean_RT1$RTZ <-scale(pptmean_RT1$RT, center = TRUE, scale = TRUE)
pptmean_RT2$RTZ <-scale(pptmean_RT2$RT, center = TRUE, scale = TRUE)

# plot response times
hist(Session1_Data$RT)
hist(Session2_Data$RT)


#Removal participants

Session1_trim1 <- subset(Session1_Data, Participant != "P083G01S02")
Session1_trim2 <- subset(Session1_trim1, Participant != "P028G02S02")
Session1_trim3 <- subset(Session1_trim2, Participant != "P076G02S01")
Session2_trim1 <- subset(Session2_Data, Participant != "P083G01S02")
Session2_trim2 <- subset(Session2_trim1, Participant != "P076G02S01")
Session2_trim3 <- subset(Session2_trim2, Participant != "P024G02S02")


#plot trimmed data

hist(Session1_trim1$RT)
hist(Session2_trim2$RT)

#rename datasets

Session1_finalData <- Session1_trim3
Session2_finalData <- Session2_trim3


Data.trimmed <- rbind(Session1_finalData, Session2_finalData)


Session1_Data_Odd <- subset(Data.trimmed, Session == 1 & Even_Odd == 1)
Session1_Data_Even <- subset(Data.trimmed, Session == 1 & Even_Odd == 0)
Session2_Data_Odd <- subset(Data.trimmed, Session == 2 & Even_Odd == 1)
Session2_Data_Even <- subset(Data.trimmed, Session == 2 & Even_Odd == 0)

#..............................Descriptive statistics for correlations


#
#
#
##
##
####---------------------------------------Split-half reliability
##
##
#
#


Data.trimmed$Probability[Data.trimmed$Probability == 2] <- "Prob"
Data.trimmed$Probability[Data.trimmed$Probability == 1] <- "Improb"

#for split-half reliability
Desc <- Data.trimmed %>%
  group_by(Participant, Session, Probability, Even_Odd) %>%
  summarise(mean = mean(RT))

Wide <- Desc %>% group_by(Participant) %>% unite(Type, Probability, Session, sep = "_") %>% spread(Type, mean)

Wide <- mutate(Wide, Difference1 = Improb_1 - Prob_1)
Wide <- mutate(Wide, Difference2 = Improb_2 - Prob_2)
Wide <- mutate(Wide, Ratio1 = (Improb_1 - Prob_1)/((Improb_1 + Prob_1)/2))
Wide <- mutate(Wide, Ratio2 = (Improb_2 - Prob_2)/((Improb_2 + Prob_2)/2))


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
cols_split <- paste0(c("Difference1", "Difference2", "Ratio1", "Ratio2"))
out_odd <- sapply(trim_odd[cols_split], remove_outliers)
trim_odd[cols_split][out_odd] <- NA

Seq.Learning_odd <- type.convert(trim_odd)


# Remove outliers Even
trim_even <- data.frame(Seq.Learning_even, stringsAsFactors = FALSE)

#identify outlier datapoints
out_even <- sapply(trim_even[cols_split], remove_outliers)
trim_even[cols_split][out_even] <- NA

Seq.Learning_even <- type.convert(trim_even)


#split-half reliability for regression slopes ----------------------------------------------------------------

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


Slope_data <- as.data.frame(list(df,df2,df3,df4) %>% reduce(full_join, by = "Participant"))
Slope_data <- Slope_data[,c("Participant", "Diff1_slopes", "Diff2_slopes", "Diff3_slopes", "Diff4_slopes")]

#Remove outliers


Slope <- data.frame(Slope_data, stringsAsFactors = FALSE)

cols_slope <- paste0(c("Diff1_slopes", "Diff2_slopes", "Diff3_slopes", "Diff4_slopes"))
out_slope <- sapply(Slope[cols_slope],remove_outliers)

Slope[cols_slope][out_slope] <- NA

Slope_trim <- type.convert(Slope)



# Difference scores --------------------------------

D1 <- cor.test(Seq.Learning_even$Difference1, Seq.Learning_odd$Difference1)
D2 <- cor.test(Seq.Learning_even$Difference2, Seq.Learning_odd$Difference2)

# Ratio scores --------------------------------------------

R1 <- cor.test(Seq.Learning_even$Ratio1, Seq.Learning_odd$Ratio1)
R2 <- cor.test(Seq.Learning_even$Ratio2, Seq.Learning_odd$Ratio2)

#Regression slopes


RS1 <- cor.test(Slope_trim$Diff1_slopes, Slope_trim$Diff2_slopes)
RS2 <- cor.test(Slope_trim$Diff3_slopes, Slope_trim$Diff4_slopes)


Split_half <- data.frame(Measure = c("Difference", "Ratio", "Regression slopes"), Split.half_1 = round(c(D1$estimate, R1$estimate, RS1$estimate),3), Split.half_2 = round(c(D2$estimate, R2$estimate, RS2$estimate),3)) 

print(Split_half)

Even <- Seq.Learning_even %>% group_by(Participant) %>% summarise(diff1 = mean(Difference1), diff2 = mean(Difference2))
Odd <- Seq.Learning_odd %>% group_by(Participant) %>% summarise(diff1 = mean(Difference1), diff2 = mean(Difference2))
Regr <- Slope_trim %>% group_by(Participant) %>% summarise(diff1 = mean(Diff1_slopes), diff2 = mean(Diff2_slopes), diff3 = mean(Diff3_slopes), diff4 = mean(Diff4_slopes))
