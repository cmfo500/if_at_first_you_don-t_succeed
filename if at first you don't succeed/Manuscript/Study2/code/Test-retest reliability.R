# Experiment 2 - Data analysis
# Manuscript title: "Reliability of the Serial Reaction Time task: If at first you don't succeed, try try try again"
# Author: "Cátia Margarida Oliveira, Marianna E. Hayiou-Thomas, Lisa Henderson; University of York"

# libraries
source("utils.R")

listOfPackages <- c("BayesFactor","purrr", "lavaan", "lavaanPlot", "tidyr", "semPlot", "tidyverse", "dplyr", "lme4", "lmerTest",
                    "ggplot2", "readxl", "Hmisc", "sjPlot", "influence.ME", "BlandAltmanLeh", "reshape", "ggExtra", "olsrr",
                    "psych", "fitdistrplus", "tidyr")
ipak(listOfPackages)

# Add retimes
retimes <- "https://cran.r-project.org/src/contrib/Archive/retimes/retimes_0.1-2.tar.gz"
install.packages(retimes, repos=NULL, type="source")

# Read in file
Data <- read.csv("data/Study2_data_all_variables.csv", header = TRUE)

# Add transformed RTs
Data$logRT <- log(Data$RT)

# Select for computing reliability for the last 600 trials
Data <- subset(Data, Epoch > 2)

Data$Probability[Data$Probability == 2] <- "Prob"
Data$Probability[Data$Probability == 1] <- "Improb"

Session1_Data <- subset(Data, Session == 1)
Session2_Data <- subset(Data, Session == 2)
Session3_Data <- subset(Data, Session == 3)

Data_results1 <- aggregate(RT ~Participant, Session1_Data, mean)
Data_results2 <- aggregate(RT ~Participant, Session2_Data, mean)
Data_results3 <- aggregate(RT ~Participant, Session3_Data, mean)

# PARTICIPANT OUTLIER REMOVAL BASED ON ACCURACY

#--- Same group

#Accuracy
pptmeanacc <- aggregate(Accuracy ~ Participant, Session1_Data, sum)
pptmeanacc2 <- aggregate(Accuracy ~ Participant, Session2_Data, sum)
pptmeanacc3 <- aggregate(Accuracy ~ Participant, Session3_Data, sum)

#Z scores

pptmeanacc$AccZ <- scale(pptmeanacc$Accuracy, center = TRUE, scale = TRUE)
pptmeanacc2$AccZ2 <- scale(pptmeanacc2$Accuracy, center = TRUE, scale = TRUE)
pptmeanacc3$AccZ3 <- scale(pptmeanacc3$Accuracy, center = TRUE, scale = TRUE)


# PARTICIPANT OUTLIER REMOVAL BASED ON Response Times
#---- Same group

# Mean response times 
pptmean_RT1 <- aggregate(RT ~ Participant, data= Session1_Data, mean)
pptmean_RT2 <- aggregate(RT ~ Participant, data= Session2_Data, mean)
pptmean_RT3 <- aggregate(RT ~ Participant, data= Session3_Data, mean)

####z scores
pptmean_RT1$RTZ <- scale(pptmean_RT1$RT, center = TRUE, scale = TRUE)
pptmean_RT2$RTZ <- scale(pptmean_RT2$RT, center = TRUE, scale = TRUE)
pptmean_RT3$RTZ <- scale(pptmean_RT3$RT, center = TRUE, scale = TRUE)

#Removal participants

Session1_trim1 <- subset(Session1_Data, Participant != "P035")
Session3_trim1 <- subset(Session3_Data, Participant != "P009")

#rename datasets

Session1_finalData <- Session1_trim1
Session3_finalData <- Session3_trim1

# Combine datasets

Data.trimmed <- rbind(Session1_finalData, Session2_Data, Session3_finalData)


# Descriptive statistics ------------------------------------

Desc <- Data.trimmed %>%
  group_by(Participant, Probability, Session) %>%
  summarise(mean = mean(RT))

Difference <- Desc %>% group_by(Participant) %>% unite(Type, Probability, Session, sep = "_") %>% spread(Type, mean)

Difference <- mutate(Difference, Diff1 = Improb_1 - Prob_1,
                     Diff2 = Improb_2 - Prob_2,
                     Diff3 = Improb_3 - Prob_3,
                     Ratio1.1 = (Improb_1 - Prob_1)/((Improb_1 + Prob_1)/2),
                     Ratio2.1 = (Improb_2 - Prob_2)/((Improb_2 + Prob_2)/2),
                     Ratio3.1 = (Improb_3 - Prob_3)/((Improb_3 + Prob_3)/2),
                     Ratio1.2 = (Improb_1 - Prob_1)/Improb_1,
                     Ratio2.2 = (Improb_2 - Prob_2)/Improb_2,
                     Ratio3.2 = (Improb_3 - Prob_3)/Improb_3)

#Language and Reading measures ------------------------------------------------------------------

#---------------------------Word reading

Word_reading <- aggregate(Word_reading ~ Participant, Data.trimmed, mean)

#---------------------------Word reading

Nonword_reading <- aggregate(Nonword_reading ~ Participant, Data.trimmed, mean)

#---------------------------Spelling

Spelling <- aggregate(Spelling ~ Participant, Data.trimmed, mean)

#--------------------------Vocabulary

Vocabulary <- aggregate(Vocabulary ~ Participant, Data.trimmed, mean)

#-------------------------- Sentence Recall/Repetition

Sentence_recall <- aggregate(Recalling ~ Participant, Data.trimmed, mean)

#-------------------------- Nonword Repetition

Nonword_Repetition <- aggregate(Nonword_repetition ~ Participant, Data.trimmed, mean)

#-------------------------- Non-verbal IQ

Matrix_Reasoning <- aggregate(Matrix_Reasoning ~ Participant, Data.trimmed, mean)

#-------------------------- Explicit Awareness ----------------------------------------------------------------------
# Different triplets

Diff_triplets <- aggregate(Diff_triplets ~ Participant, Data.trimmed, mean)

# Longest sequence

Longest_sequence <- aggregate(Longest ~ Participant, Data.trimmed, mean)

# Total triplets

total_triplet <- aggregate(Triplet_Match ~ Participant, Data.trimmed, mean)

# Attention ----------------------------------------------------------------

Data_PVT <- read.csv("data/PVT_results.csv", header = TRUE)
Data_PVT <- subset(Data_PVT, RT != 0)

#Reciprocal

Data_PVT$Reciprocal <- 1/(Data_PVT$RT/1000)

PVT_reciprocal <- aggregate(Reciprocal ~ Participant, Data_PVT, mean)

#Lapses 

PVT_lapses <- aggregate(PVT_lapses ~ Participant, Data.trimmed, mean)

#Median 

PVT_median <- Data_PVT %>% group_by(Participant) %>% summarise(median_PVT = median(RT))

#Correlations ----------------------------------------

Age <- aggregate(Age ~ Participant, data = Data.trimmed, mean)

Enjoyment <- aggregate(Enjoyment ~ Participant, data = Data.trimmed, mean)

#Cor_data <- as.data.frame(list(Difference, Word_reading, Nonword_reading, Spelling, Vocabulary, Nonword_Repetition, Sentence_recall, Matrix_Reasoning, Diff_triplets, Longest_sequence, total_triplet, PVT_reciprocal, PVT_lapses) %>% reduce(full_join, by = "Participant"))
Cor <- as.data.frame(list(Difference,Age, Word_reading, Nonword_reading, Spelling, Vocabulary, Nonword_Repetition,
                          Sentence_recall, Matrix_Reasoning, Diff_triplets, Longest_sequence, total_triplet, PVT_reciprocal,
                          PVT_lapses, PVT_median,Enjoyment) %>% reduce(full_join, by = "Participant"))

#Remove outliers for all variables

# Remove outliers Odd
trim <- data.frame(Cor, stringsAsFactors = FALSE)

#identify outlier datapoints
cols <- paste0(c("Diff1", "Diff2", "Diff3", "Ratio1.1", "Ratio2.1", "Ratio3.1", "Ratio1.2", "Ratio2.2", "Ratio3.2",
                 "Word_reading", "Nonword_reading", "Spelling", "Vocabulary", "Nonword_repetition", "Recalling",
                 "Matrix_Reasoning", "Diff_triplets", "Longest", "Triplet_Match", "Reciprocal", "PVT_lapses",
                 "median_PVT", "Enjoyment"))
out <- sapply(trim[cols], remove_outliers)
trim[cols][out] <- NA

Cor_data <- type.convert(trim)

#Regression graphs ----------------------------------------------------------------

#Diff and attention
model <- lm(Diff2 ~ Reciprocal, data = Cor_data)

summary(model)


plot(Cor_data$Reciprocal, Cor_data$Diff2, main = "Regression")
abline(model, col = "lightblue")

#95% interval for regression

summary(Cor_data$Reciprocal)

range <- seq(2.902, 4.566, by = 0.05)
plot(Cor_data$Reciprocal, Cor_data$Diff2, main = "Regression")
abline(model, col = "lightblue")

conf.int <- predict(model, newdata = data.frame(Reciprocal = range), interval = "confidence", level = 0.95)
lines(range, conf.int[,2], col = "orange", lty = 2 )
lines(range, conf.int[,3], col = "orange", lty = 2 )

ggplot(Cor_data, aes(x = Reciprocal, y = Diff2)) +
  theme_classic() + geom_point() +
  geom_smooth(method = lm, se = TRUE, color = "black", fill = "light blue")

#individual slopes ---------------------------------------

Data.trimmed$Probability[Data.trimmed$Probability == "Prob"] <- "0"
Data.trimmed$Probability[Data.trimmed$Probability == "Improb"] <- "1"

Session1_Data <- subset(Data.trimmed, Session == 1)
Session2_Data <- subset(Data.trimmed, Session == 2)
Session3_Data <- subset(Data.trimmed, Session == 3)

#session 1
model1 <- lmer(logRT ~ Probability + (Probability|Participant),data= Session1_Data, REML=FALSE)

df.slopes <- coef(model1)$Participant[2]
df.slopes <- tibble::rownames_to_column(df.slopes, "Participant") 
names(df.slopes)[names(df.slopes) == "Probability1"] <- "Diff1_slopes"

# session 2
model2 <- lmer(logRT ~ Probability + (Probability|Participant),data= Session2_Data, REML=FALSE)

df2.slopes <- coef(model2)$Participant[2]
df2.slopes <- tibble::rownames_to_column(df2.slopes, "Participant")
names(df2.slopes)[names(df2.slopes) == "Probability1"] <- "Diff2_slopes"

#session 3
model3 <- lmer(logRT ~ Probability + (Probability|Participant),data= Session3_Data, REML=FALSE)

df3.slopes<- coef(model3)$Participant[2]
df3.slopes <- tibble::rownames_to_column(df3.slopes, "Participant")
names(df3.slopes)[names(df3.slopes) == "Probability1"] <- "Diff3_slopes"

#all sessions
model.total <- lmer(logRT ~ Probability + (Probability|Participant),data= Data.trimmed, REML=FALSE)

df.total.slopes<- coef(model.total)$Participant[2]
df.total.slopes <- tibble::rownames_to_column(df.total.slopes, "Participant")
names(df.total.slopes)[names(df.total.slopes) == "Probability1"] <- "Diff.total_slopes"

Slopes <- as.data.frame(list(df.slopes, df2.slopes, df3.slopes, df.total.slopes) %>% reduce(full_join, by = "Participant"))

#remove outliers

# Remove outliers Odd
trim_slopes <- data.frame(Slopes, stringsAsFactors = FALSE)

#identify outlier datapoints
cols_slopes <- paste0(c("Diff1_slopes", "Diff2_slopes", "Diff3_slopes", "Diff.total_slopes"))
out_slopes <- sapply(trim_slopes[cols_slopes], remove_outliers)
trim_slopes[cols_slopes][out_slopes] <- NA

Slopes <- type.convert(trim_slopes)

# Ex-Gaussian with PVT task--------------------

Ex_gaussian_PVT_mu <- as.data.frame(Data_PVT %>% group_by(Participant) %>% summarise(Ex_gaussian_PVT_mu = retimes::mexgauss(RT)[1]))
Ex_gaussian_PVT_sigma <- as.data.frame(Data_PVT %>% group_by(Participant) %>% summarise(Ex_gaussian_PVT_sigma = retimes::mexgauss(RT)[2]))
Ex_gaussian_PVT_tau <- as.data.frame(Data_PVT %>% group_by(Participant) %>% summarise(Ex_gaussian_PVT_tau = retimes::mexgauss(RT)[3]))
Ex_gaussian_PVT_tau$scale <- scale(Ex_gaussian_PVT_tau$Ex_gaussian_PVT_tau)
Ex_gaussian_PVT_tau <- subset(Ex_gaussian_PVT_tau, scale < 2.5)
Ex_gaussian_PVT_tau$scale <- NULL

mean(Ex_gaussian_PVT_tau$Ex_gaussian_PVT_tau)
sd(Ex_gaussian_PVT_tau$Ex_gaussian_PVT_tau)

Complete_data <- as.data.frame(list(Ex_gaussian_PVT_mu, Ex_gaussian_PVT_sigma, Ex_gaussian_PVT_tau, Slopes, Cor_data) %>%
                                 reduce(full_join, by = "Participant"))

# Correlations matrix

res2 <- rcorr(as.matrix(Complete_data[-1]), type = "pearson")
Res2_cor <- as.data.frame(res2$r)
Res2_p <- as.data.frame(res2$P)

write.csv(Res2_cor, "results/correlations.csv")

write.csv(Res2_p, "results/correlations_pvalues.csv")

median(na.omit(Complete_data$Ex_gaussian_PVT_tau))

Complete_low_tau <- subset(Complete_data, Ex_gaussian_PVT_tau <= 59.54)

Complete_high_tau <- subset(Complete_data, Ex_gaussian_PVT_tau > 59.54)

tau_cor_low <- as.data.frame(cor(Complete_low_tau[-1], method = "pearson", use = "pairwise.complete.obs"))

tau_cor_high <- as.data.frame(cor(Complete_high_tau[-1], method = "pearson", use = "pairwise.complete.obs"))

# Does attention predict reliability?

attention.model1 <- lm(scale(Diff2_slopes) ~ scale(Diff1_slopes)*scale(Ex_gaussian_PVT_tau), data = Complete_data)
summary(attention.model1)

attention.model2 <- lm(scale(Diff3_slopes) ~ scale(Diff2_slopes)*scale(Ex_gaussian_PVT_tau), data = Complete_data)
summary(attention.model2)

#Create table with test-retest reliability summary -----------------------

D12 <- cor.test(Complete_data$Diff1, Complete_data$Diff2)
D23 <- cor.test(Complete_data$Diff2, Complete_data$Diff3) 
R12.1 <- cor.test(Complete_data$Ratio1.1, Complete_data$Ratio2.1) 
R23.1 <- cor.test(Complete_data$Ratio2.1, Complete_data$Ratio3.1)
R12.2 <- cor.test(Complete_data$Ratio1.2, Complete_data$Ratio2.2) 
R23.2 <- cor.test(Complete_data$Ratio2.2, Complete_data$Ratio3.2) 
RS12 <- cor.test(Complete_data$Diff1_slopes, Complete_data$Diff2_slopes)
RS23 <- cor.test(Complete_data$Diff2_slopes, Complete_data$Diff3_slopes)

Test_retest <- data.frame(Measure = c("Difference", "Ratio.1", "Ratio.2", "Regression slopes"),
                          Test_retest_S1_S2 = round(c(D12$estimate, R12.1$estimate, R12.2$estimate, RS12$estimate),3),
                          Test_retest_S2_S3 = round(c(D23$estimate, R12.1$estimate, R12.2$estimate, RS23$estimate),3))
print(Test_retest)

colSums(!is.na(Complete_data))

# Bland-Altman agreement ---------------------------------------

#agreement
ba.stats_12 <- bland.altman.stats(Slopes$Diff2_slopes, Slopes$Diff1_slopes)

Bland_Altman_12 <- print(ggMarginal((bland.altman.plot(Slopes$Diff2_slopes, Slopes$Diff1_slopes,
                                                       two = 1.96, mode = 1, graph.sys = "ggplot2",
                                                       conf.int = .95, pch = 19, ylim = c(-0.00, 0.10)) +
                                                       labs(y = "Difference in procedural learning between sessions (S2-S1)")),
                                                       type = "histogram", size=4))

ba.stats_23 <- bland.altman.stats(Slopes$Diff3_slopes, Slopes$Diff2_slopes)

Bland_Altman_23 <- print(ggMarginal((bland.altman.plot(Slopes$Diff3_slopes, Slopes$Diff2_slopes,
                                                       two = 1.96, mode = 1, graph.sys = "ggplot2",
                                                       conf.int = .95, pch = 19, ylim = c(-0.050, 0.10)) +
                                                       labs(y = "Difference in procedural learning between sessions(S3-S2)")),
                                                       type = "histogram", size=4))

#Bland-Atlman with adjustable axis ----------------------------------------

#session 1 and 2

ba12 <- bland.altman.stats(Complete_data$Diff2, Complete_data$Diff1)
summary(ba12$CI.lines)
ba12$lines
ba12$CI.lines
ba.stats12 <- as.data.frame(cbind(ba12$means, ba12$diffs))

BA12 <- ggMarginal(ggplot(ba.stats12, aes(V1,V2)) +
                     geom_hline(yintercept = ba12$lines, linetype="dashed", size=1, col = "black") +
                     geom_point(colour = "dodgerblue3", size = 2.5) +
                     theme_bw()+theme(legend.position = "bottom",
                     legend.box = "vertical") + labs(color = "Levenshtein distance  ") +
                     scale_y_continuous(breaks = seq(-70,70,20)) +
                     scale_x_continuous(breaks = seq(20,70,10)) +
                     coord_cartesian(ylim = c(-68,68))+
                     labs(x = "\n Average procedural learning", y = "Difference in procedural learning between sessions (S2-S1)") +
                     geom_hline(yintercept = ba12$CI.lines, col = "grey", linetype="dashed", size = 0.70), type = "histogram")

BA12

#session 2 and 3

ba23 <- bland.altman.stats(Complete_data$Diff3, Complete_data$Diff2)
summary(ba23$CI.lines)
ba23$lines
ba23$CI.lines
ba.stats23 <- as.data.frame(cbind(ba23$means, ba23$diffs))

BA23 <- ggMarginal(ggplot(ba.stats23, aes(V1,V2)) +
                     geom_hline(yintercept = ba23$lines, linetype="dashed", size=1, col = "black") +
                     geom_point(colour = "dodgerblue3", size = 2.5) +
                     theme_bw() + theme(legend.position = "bottom",
                     legend.box = "vertical") + labs(color = "Levenshtein distance  ") +
                     scale_y_continuous(breaks = seq(-70,70,20)) +
                     scale_x_continuous(breaks = seq(20,70,10)) +
                     coord_cartesian(ylim = c(-68,68)) +
                     labs(x = "\n Average procedural learning",
                          y = "Difference in procedural learning between sessions (S3-S2)") +
                     geom_hline(yintercept = ba23$CI.lines, col = "grey", linetype="dashed", size = 0.70), type = "histogram")

BA23

grid <- plot_grid(BA12, BA23, ncol = 2, labels = c("A", "B"))

grid

ggsave("plots/Bland_Altman.plots.png", dpi = 1400, height = 8, width = 12, bg = "white")

#Explicit awareness comparison with simulation -------------------------------------

total_triplet <- aggregate(Triplet_Match ~ Participant, Data.trimmed, mean)

t.test(Diff_triplets$Diff_triplets, mu = 10.40, alternative = "two.sided")
t.test(Longest_sequence$Longest, mu = 5.84, alternative = "two.sided")
t.test(total_triplet$Triple_Match, mu = 29.96, alternative = "two.sided")

hist(Diff_triplets$Diff_triplets)
hist(Longest_sequence$Longest)

# ICC reliability --------------------------------------

library(irr)
#Slopes$Participant <- NULL

icc(Slopes, model = c("twoway"),
    type = c("agreement"),
    unit = c("single"), r0 = 0, conf.level = 0.95)

library(dplyr)

df <- Difference[,c("Participant", "Diff1", "Diff2")]
df_slopes <- Slopes[,c("Diff1_slopes", "Diff2_slopes", "Diff3_slopes", "Diff.total_slopes")]

#Bayesian correlations ----------------------

# Select variables for Bayesian correlations
BF.cols <- c(c(4:8), c(24:38))
df <- Complete_data[BF.cols]

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
Bayes.factors <- expand.grid(V1 =names(df), V2=names(df))%>%
  filter(V1 != V2) %>%   # keep pairs of names where v1 matches v2, but are not the same
  mutate(BF = BFcor(V1, V2), lb.CI = lb.CIcor(V1,V2), ub.CI = ub.CIcor(V1,V2)) %>%   # for those pairs (only) obtain correlation value
  subset(V1 %in% names(df_slopes))

write.csv(Bayes.factors, "results/Bayes.factors.csv")

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

RT.plot <- RT.plot + ylab("mean RT") + xlab("\nEpoch")

# Save plot
print(RT.plot)
ggsave("plots/Procedural.learning.S2.png", dpi = 1000, height = 5, width = 7, bg = "white")

# Plotting procedural learning across sessions ------------------

Diff_long <- Complete_data %>% dplyr::select(c('Participant', "Diff1", "Diff2", "Diff3"))
Diff_long <- Diff_long %>% gather(Session, Diff, Diff1:Diff3, factor_key=TRUE)

ggplot(Diff_long, aes(x= as.factor(Session), y= Diff, group = Participant, colour = Participant)) +
  geom_point()+
  geom_line(alpha = .8, size = 1)+
  theme_classic() +
  scale_x_discrete(name ="\n Session", labels=c("1","2","3")) +
  theme(legend.position="none")
