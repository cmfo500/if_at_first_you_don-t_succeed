# manuscript title: "Experiment 1: Reliability of the Serial Reaction Time task: If at first you don't succeed, try try try again"
# authors: "Catia Oliveira, Marianna E. Hayiou-Thomas, Lisa Henderson, University of York"

library(BlandAltmanLeh)
library(tidyverse)
library(magrittr)
library(lme4)
library(irr)
library(ggExtra)
library(dplyr)
library(BayesFactor)

# Read in file
Data <- read.csv("data/Data_final_RT.csv", header = TRUE)

Data$logRT <- log(Data$RT)

#Data <- subset(Data, Epoch > 2)
#separate sessions

Session1_Data <- subset(Data, Session == 1)
Session2_Data <- subset(Data, Session == 2)

Session1_Prob <- subset(Session1_Data, Probability == 2)
Session1_Improb <- subset(Session1_Data, Probability == 1)
Session2_Prob <- subset(Session2_Data, Probability == 2)
Session2_Improb <- subset(Session2_Data, Probability == 1)

Data_results1 <- aggregate(RT ~Participant, Session1_Data, mean)
Data_results2 <- aggregate(RT ~Participant, Session2_Data, mean)

#------------------------------------

# PARTICIPANT OUTLIER REMOVAL BASED ON ACCURACY

#Accuracy
pptmeanacc <-aggregate(Accuracy ~ Participant, data = Session1_Data, sum)
pptmeanacc2 <-aggregate(Accuracy ~ Participant, data = Session2_Data, sum)

#Z scores

pptmeanacc$AccZ <-scale(pptmeanacc$Acc, center = TRUE, scale = TRUE)
pptmeanacc2$AccZ <-scale(pptmeanacc2$Acc, center = TRUE, scale = TRUE)

# PARTICIPANT OUTLIER REMOVAL BASED ON Response Times

# Mean response times 
pptmean_RT1 <-aggregate(RT ~ Participant, data= Session1_Data, mean)
pptmean_RT2 <-aggregate(RT ~ Participant, data= Session2_Data, mean)

####z scores
pptmean_RT1$RTZ <-scale(pptmean_RT1$RT, center = TRUE, scale = TRUE)
pptmean_RT2$RTZ <-scale(pptmean_RT2$RT, center = TRUE, scale = TRUE)

#Removal outlier participants

#Identify outlier participants based on RTs TD group
OutRT1 <- subset(pptmean_RT1, RTZ < -2.5|RTZ > 2.5) %>% dplyr::select(Participant)
OutRT2 <- subset(pptmean_RT2, RTZ < -2.5|RTZ > 2.5) %>% dplyr::select(Participant)

#removal per session
Session_1_trim <- subset(Session1_Data, !(Participant  %in%  OutRT1$Participant))
Session_2_trim <- subset(Session2_Data, !(Participant %in%  OutRT2$Participant))

# final dataset response times model
Data.trimmed <- rbind(Session_1_trim, Session_2_trim)

#Check missing data points
#options(max.print = 200000)
#which(is.na(Data.trimmed) == T)


# Descriptive statistics ------------------------------------
Data.trimmed$Probability[Data.trimmed$Probability == 2] <- "Prob"
Data.trimmed$Probability[Data.trimmed$Probability == 1] <- "Improb"

Desc <- Data.trimmed %>%
  dplyr::group_by(Participant, Session, Probability) %>%
  summarise(mean = mean(RT))

Wide <- Desc %>% group_by(Participant) %>% unite(Type, Probability, Session, sep = "_") %>% spread(Type, mean)

Wide <- mutate(Wide, Difference1 = Improb_1 - Prob_1)
Wide <- mutate(Wide, Difference2 = Improb_2 - Prob_2)
Wide <- mutate(Wide, Ratio1.1 = (Improb_1 - Prob_1)/((Improb_1 + Prob_1)/2))
Wide <- mutate(Wide, Ratio2.1 = (Improb_2 - Prob_2)/((Improb_2 + Prob_2)/2))
Wide <- mutate(Wide, Ratio1.2 = (Improb_1 - Prob_1)/Improb_1)
Wide <- mutate(Wide, Ratio2.2 = (Improb_2 - Prob_2)/Improb_2)

mean(na.omit(Wide$Difference1))
mean(na.omit(Wide$Difference2))

sd(na.omit(Wide$Difference1))
sd(na.omit(Wide$Difference2))

Data.trimmed$Probability[Data.trimmed$Probability == "Prob"] <- "0"
Data.trimmed$Probability[Data.trimmed$Probability == "Improb"] <- "1"

#regression slopes ----------------------------------------------

Session1_Data <- filter(Data.trimmed, Session == 1)
Session2_Data <- filter(Data.trimmed, Session == 2)

# linear mixed model

#session 1
model1 <- lmer(logRT ~ Probability + (Probability|Participant),data= Session1_Data, REML=FALSE)

df <- coef(model1)$Participant[2]
df <- tibble::rownames_to_column(df, "Participant") 
names(df)[names(df) == "Probability1"] <- "Diff1_slopes"

# session 2
model2 <- lmer(logRT ~ Probability + (Probability|Participant),data= Session2_Data, REML=FALSE)

df2 <- coef(model2)$Participant[2]
df2 <- tibble::rownames_to_column(df2, "Participant")
names(df2)[names(df2) == "Probability1"] <- "Diff2_slopes"

# gamma mixed model

#session 1
model1.gamma <- glmer(logRT ~ Probability + (Probability | Participant), family = Gamma, data = Session1_Data,
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
df.gamma <- coef(model1.gamma)$Participant[2]
df.gamma <- tibble::rownames_to_column(df.gamma, "Participant") 
names(df.gamma)[names(df.gamma) == "Probability1"] <- "Diff1_slopes.gamma"

# session 2
model2.gamma <- glmer(logRT ~ Probability + (Probability | Participant), family = Gamma, data = Session2_Data,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
df2.gamma <- coef(model2.gamma)$Participant[2]
df2.gamma <- tibble::rownames_to_column(df2.gamma, "Participant")
names(df2.gamma)[names(df2.gamma) == "Probability1"] <- "Diff2_slopes.gamma"

#combine difference scores in dataframe

Difference <- as.data.frame(list(Wide, df, df2, df.gamma, df2.gamma) %>% reduce(full_join, by = "Participant"))

#individual differences --------------------------------------------------

#explicit awareness measures

## Total triplets
Triplets <- aggregate(Triplet_Match ~ Participant, Data.trimmed, mean) %>% rename(Triplets = Triplet_Match)

## diff-triplets
Diff_Triplets <- aggregate(Diff_triplets ~ Participant, Data.trimmed, mean)

##longest sequence
Longest <- aggregate(Longest ~ Participant, Data.trimmed, mean)

#enjoyment
Enjoyment <- aggregate(Enjoyment ~ Participant, Data.trimmed, mean)

#similarity
Distance <- aggregate(Distance ~ Participant, Data.trimmed, mean)

#combine difference scores and individual differences variables in dataframe

Cor <- as.data.frame(list(Difference, Triplets, Diff_Triplets, Longest, Enjoyment, Distance) %>% reduce(full_join, by = "Participant"))

#remove outliers

#eliminate outlier datapoints

remove_outliers <-  function(x) {
  mn <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  (x > mn + sd * 2.5) | (x < mn - sd * 2.5)
}

trim <- data.frame(Cor, stringsAsFactors = FALSE)

#identify outlier datapoints
cols_split <- paste0(c("Difference1", "Difference2", "Ratio1.1", "Ratio1.2", "Ratio2.1", "Ratio2.2", "Diff1_slopes", "Diff2_slopes",
                       "Diff1_slopes.gamma", "Diff2_slopes.gamma", "Triplets", "Diff_triplets", "Longest", "Enjoyment"))
out <- sapply(trim[cols_split], remove_outliers)
trim[cols_split][out] <- NA

Cor_data <- type.convert(trim)

#Correlations and test-retest reliability ------------------------------------------------

res <- Hmisc::rcorr(as.matrix(Cor_data[-1]))
pvalues <- round(res$P, 3)
cor <- round(res$r, 3)
res <- as.data.frame(cor(Cor_data[-1], method = "pearson", use = "pairwise.complete.obs"))

#save correlations - csv files
write.csv(cor, 'results/correlations_study1.csv')
write.csv(pvalues, 'results/pvalues.cor_study1.csv')

colSums(!is.na(Cor_data))

# Test-retest reliability
cor.test(Cor_data$Diff1_slopes, Cor_data$Diff2_slopes)

#correlation between slopes and distance to get CI around correlation
cor.test(Cor_data$Diff1_slopes, Cor_data$Distance)
cor.test(Cor_data$Diff2_slopes, Cor_data$Distance)

#Correlation between explicit awareness and similarity
cor.test(Cor_data$Diff_triplets, Cor_data$Distance)

summary(Cor_data$Diff1_slopes)
sd(na.omit(Cor_data$Diff1_slopes))

#Bayesian correlations ----------------------

# Select variables for Bayesian correlations
#BF.cols <- c("Diff1_slopes", "Diff2_slopes", "Triplets", "Diff_triplets", "Longest", "Enjoyment", "Distance")
df <- Cor_data[BF.cols]

# function to get correlation between two variables
BFcor = function(x,y) extractBF(correlationBF(BFcor.df[,x], BFcor.df[,y]))$bf
BFcor = Vectorize(BFcor)

# function to get correlation between two variables
BFcor = function(x,y) extractBF(correlationBF(df[,x], df[,y]))$bf
BFcor = Vectorize(BFcor)

# function to get lower credible interval between two variables
lb.CIcor = function(x,y) summary(correlationBF(x = df[,x], y = df[,y],
                                               posterior = TRUE, iterations = 100000))$quantiles[[1]]
lb.CIcor = Vectorize(lb.CIcor)


# function to get upper credible interval between two variables
ub.CIcor = function(x,y) summary(correlationBF(x = df[,x], y = df[,y],
                                               posterior = TRUE, iterations = 100000))$quantiles[[9]]
ub.CIcor = Vectorize(ub.CIcor)

# Compute correlations for all pairwise variables of interest
Bayes.factors.S1 <- expand.grid(V1 =names(df), V2=names(df))%>%
  filter(V1 != V2) %>%   # keep pairs of names where v1 matches v2, but are not the same
  mutate(BF = BFcor(V1, V2), lb.CI = lb.CIcor(V1,V2), ub.CI = ub.CIcor(V1,V2)) %>%   # for those pairs (only) obtain correlation value
  subset(V1 == "Diff1_slopes"|V1 == "Diff2_slopes"| V1 == "Diff3_slopes"| V1 == "Distance")


sample <- correlationBF(x = df$Diff1_slopes, y = df$Diff2_slopes,
                                 posterior = TRUE, iterations = 100000)

plot(samples[,"rho"])

write.csv(Bayes.factors.S1, "results/Bayes.factors_S1.csv")

#Plotting distance x diff

# Save plot
png("plots/Correl.distance.PL.png", bg = "white", width = 8, height = 8, units = 'in', res = 600)

plot(Cor_data$Distance, Cor_data$Diff1_slopes, col="black", ylim=c(-.02,.18), xlab="Levenshtein Distance", ylab="RT difference", pch = 1)
abline(lm(Cor_data$Diff1_slopes ~ Cor_data$Distance))
par(new=TRUE)
plot(Cor_data$Distance, Cor_data$Diff2_slopes, col="#0066CC" , ylim=c(-.02,.18), xlab="", ylab="", pch = 16)
abline(lm(Cor_data$Diff2_slopes ~ Cor_data$Distance), col = "#0066CC")
legend("topright", c("Session 1","Session 2"), xpd = TRUE, horiz = TRUE, inset = c(0,0), bty = "n", pch = c(1, 16), col = c("black", "#0066CC"), cex = 0.8)

dev.off()

#lmer with similarity

model.rel <- lm(scale(Diff2_slopes) ~ scale(Diff1_slopes), data=Cor_data)

summary(model.rel)

model.dist <- lm(scale(Diff2_slopes) ~ scale(Diff1_slopes)*scale(Distance), data=Cor_data)

summary(model.dist)
confint(model.dist)

anova(model.rel, model.dist)

# We found no evidence that age influenced test-retest reliability as indicated by non-significant the interaction
# between procedural learning in Session 1 and distance, β=0.061, SE=0.089, t = 0.688, p = 0.493.

#test-retest reliability per high and low similarity groups ---------------------

#low group---------
median(na.omit(Cor_data$Distance))

Data.low <- subset(Cor_data, Distance > 396)
#Data.low <- scale(Data.low[9:10]) #1 outlier

#remove outlier
trim_low <- data.frame(Data.low, stringsAsFactors = FALSE)

#identify and remove outlier datapoints
cols_low <- paste0(c("Difference1", "Difference2", "Diff1_slopes", "Diff2_slopes"))
out_low <- sapply(trim_low[cols_low], remove_outliers)
trim_low[cols_low][out_low] <- NA

Data.low <- type.convert(trim_low)

Data.low$Participant <- NULL
low_sim <- as.data.frame(cor(Data.low, method = "pearson", use = "pairwise.complete.obs"))

cor.test(Data.low$Diff1_slopes, Data.low$Diff2_slopes)

#descriptive statistics
summary(Data.low)
sd(Data.low$Diff1_slopes)
sd(na.omit(Data.low$Diff2_slopes))

colSums(!is.na(Data.low))

#high group-------------------
Data.high <- subset(Cor_data, Distance <= 396) 
#Data.high <- scale(Data.high[9:10]) #no outliers

Data.high$Participant <- NULL
high_sim <- as.data.frame(cor(Data.high, method = "pearson", use = "pairwise.complete.obs"))

#descriptive statistics

summary(na.omit(Data.high))
sd(na.omit(Data.high$Diff1_slopes))
sd(na.omit(Data.high$Diff2_slopes))

cor.test(Data.high$Diff1_slopes, Data.high$Diff2_slopes)

colSums(!is.na(Data.high))


#Comparison of explicit awareness scores with simulation --------------------------------------------

t.test(Triplets$Triplets, mu = 29.98, alternative = "two.sided")
t.test(Diff_Triplets$Diff_triplets, mu = 10.40, alternative = "two.sided")
t.test(Longest$Longest, mu = 5.84, alternative = "two.sided")


df <- Cor_data[,c("Participant", "Difference1", "Difference2", "Diff1_slopes", "Diff2_slopes", "Distance")]

data_long <- gather(df, Session, Difference, Difference1:Difference2, factor_key=TRUE)

anova(lm(Difference ~ Session + Participant, data_long))

df$Participant <- NULL

ICC(df,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)
icc(df, model = c("twoway"), 
    type = c("agreement"), 
    unit = c("average"), r0 = 0, conf.level = 0.95)


fit0 <- lmer(Difference ~ Session + (1| Participant), data_long)
performance::icc(fit0)


# Bland-Altman agreement ---------------------------------------

#agreement
ba.stats_12 <- bland.altman.stats(Cor_data$Difference1, Cor_data$Difference2)

Bland_Altman_12 <- print(ggMarginal((bland.altman.plot(Cor_data$Difference1, Cor_data$Difference2, two = 1.96, mode = 1, graph.sys = "ggplot2", conf.int = .95, pch = 19, ylim = c(-0.00, 0.10)) + labs(y = "Difference (S2-S1)")),
                                    type = "histogram", size=4))


# Bland-Altman with adjustable axis

dots <- df[complete.cases(df), ]# remove participants who failed to complete both sessions

ba <- bland.altman.stats(dots$Difference1, dots$Difference2)
ba$lines
summary(ba$CI.lines)

ba.stats2 <- as.data.frame(cbind(ba$means, ba$diffs, dots$Distance))


BA <- ggMarginal(ggplot(ba.stats2, aes(V1,V2, col = V3)) + geom_point(size = 2.5) + theme_bw()+theme(legend.position = "bottom",
                    legend.box = "vertical") + labs(color = "Levenshtein distance  ")+
                     geom_hline(yintercept = ba$lines, linetype="dashed", size=1, col = "black")+
                   scale_y_continuous(breaks = seq(-70,70,20))+  coord_cartesian(ylim = c(-68,68))+
                     labs(x = "\n Average procedural learning", y = "Difference (S3-S2)")+
                     geom_hline(yintercept = ba$CI.lines, col = "grey", linetype="dashed", size = 0.70), type = "histogram")

# Save plot
tiff("Bland_Altman.plot.tiff", units="in", width=7, height=5, res=300)
BA
dev.off()

#Plotting ----------------

RT <- Data.trimmed %>%
  group_by(Probability, Epoch, Session) %>%
  summarise(mean = mean(RT, na.rm = TRUE),
            sd = sd(RT, na.rm = TRUE),
            n = length(which(!is.na(RT)))) %>%
  mutate(se = sd / sqrt(n),
         lower.ci= mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

RT$Probability <- as.numeric(RT$Probability)
RT$Probability[RT$Probability == 0] <- "Prob"
RT$Probability[RT$Probability == 1] <- "Improb"

RT$Epoch <- as.factor(RT$Epoch)

RT.plot <- ggplot(RT, aes(x= Epoch, y= mean, group = Probability, color=Probability))+ 
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci), width=.05, 
                position=position_dodge(0.05),
                show.legend = FALSE)  + xlab("Epoch") +
  geom_line(aes(linetype=Probability), size = 1) + 
  geom_point(aes(shape=Probability))+
  facet_wrap(~ Session, scales="free") + coord_cartesian(ylim = c(350, 520)) + scale_color_grey(start=0.85, end=0.2)+
  theme_classic()

RT.plot <- RT.plot + ylab("mean RT") + xlab("Epoch")

# Save plot
tiff("RT.plot.tiff", units="in", width=7, height=5, res=300)
print(RT.plot)
dev.off()

#Descriptives table - RT

library(data.table)
Desc.table <- dcast(setDT(RT), Epoch~Probability+Session, value.var=c('mean', "sd"))
Desc.table <- round(Desc.table[,2:9],2)
write.csv(Desc.table, "results/desc_table.csv")

#plot levenshtein distance distribution

# Save plot
png("plots/Distance_distribution.png",  width = 750, height = 675)
distance.S1 <- hist(Cor_data$Distance, col = "darkcyan", xlab = "LD distance", ylab = "", breaks = seq(min(na.omit(Cor_data$Distance)), max(na.omit(Cor_data$Distance)), length.out = 11), xlim = c(248, 450), main = "")

distance.S1 <- ggplot(Cor_data, aes(x=Distance)) +
  geom_histogram(color="black", fill="darkcyan") + 
  scale_x_continuous("\nLD distance - Experiment 1") +
  coord_cartesian(xlim = c(250, 450)) +
  scale_y_continuous("Frequency\n") +
  theme_classic()
distance.S1

dev.off()


