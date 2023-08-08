# Manuscript title: "Reliability of the Serial Reaction Time task: If at first you don't succeed, try try try again"
# Author: "C?tia Margarida Oliveira, Marianna E. Hayiou-Thomas, Lisa Henderson; University of York"

# This script was adapted from the analyses performed by Haines et al. (2020). Theoretically Informed Generative Models Can Advance the Psychological and Brain Sciences: Lessons from the Reliability Paradox. PsyArXiv. https://doi.org/10.31234/osf.io/xr7y3


library(fitdistrplus) 
library(philentropy)
library(foreach)
library(tidyverse)

# Read in file
Data.trimmed <- read.csv("data/SuppExp_trimmed.csv", header = TRUE)

Data.trimmed <- subset(Data.trimmed, Participant != "SF5990")

Data.trimmed <- subset(Data.trimmed, Group == "ISI") # change to noISI for results for noISI group

keep <- Data.trimmed %>% filter(Session == 2) %>% pull(Participant)

Data.trimmed <- Data.trimmed %>% filter(Participant %in% keep)

Data.trimmed <- subset(Data.trimmed, Epoch > 2)

Participant <- Data.trimmed %>%
  group_by(Participant) %>%
  summarise(Participant = Participant[2])

n_subj <- length(Participant$Participant)

results <- foreach(i=1:n_subj, .combine = "rbind") %do% {
  # Simulate RT distributions for conditions and timepoints
  j <- Participant[i,]

  
  datap1 <- subset(Data.trimmed, Participant == as.character(j) & Probability == "Prob" & Session == 1)
  datap2 <- subset(Data.trimmed, Participant == as.character(j) & Probability == "Prob" & Session == 2)
  datai1 <- subset(Data.trimmed, Participant == as.character(j) & Probability == "Improb"  & Session == 1)
  datai2 <- subset(Data.trimmed, Participant == as.character(j) & Probability == "Improb" & Session == 2)
  
  c1_t1 <- datap1[['RT']]
  c2_t1 <- datai1[['RT']]
  c1_t2 <- datap2[['RT']]
  c2_t2 <- datai2[['RT']]
  
  # Minimum and maximum RT for the density calculation
  minRT <- min(c(c1_t1, c2_t1, c1_t2, c2_t2))
  maxRT <- max(c(c1_t1, c2_t1, c1_t2, c2_t2))
  
  # For time 1, calculate mean difference and Jensen-Shannon Divergence (symmetric analogue of KLD)
  d1 <- density(c1_t1, n = 50, from = minRT, to = maxRT)$y
  d2 <- density(c2_t1, n = 50, from = minRT, to = maxRT)$y
  h1 <- d1/(sum(d1))
  h2 <- d2/(sum(d2))
  h1_a <- h1[which(h1>0 & h2>0)]
  h2_a <- h2[which(h1>0 & h2>0)]
  mu_diff_t1 <- mean(c2_t1) - mean(c1_t1)
  KLD_t1 <- sum(h2_a*log10(h2_a/h1_a))
  JSD_t1 <- JSD(rbind(h1_a, h2_a))
  JSD_t1 <- JSD(rbind(h1, h2))
  
  # For timepoint 2 (same exact individual-level parameters)
  d1 <- density(c1_t2, n = 50, from = minRT, to = maxRT)$y
  d2 <- density(c2_t2, n = 50, from = minRT, to = maxRT)$y
  h1 <- d1/(sum(d1))
  h2 <- d2/(sum(d2))
  h1_a <- h1[which(h1>0 & h2>0)]
  h2_a <- h2[which(h1>0 & h2>0)]
  mu_diff_t2 <- mean(c2_t2) - mean(c1_t2)
  KLD_t2 <- sum(h2_a*log10(h2_a/h1_a))
  JSD_t2 <- JSD(rbind(h1_a, h2_a))
  JSD_t2 <- JSD(rbind(h1, h2))
  
  # Save out data
  data.frame(mu_diff_t1 = mu_diff_t1,
             mu_diff_t2 = mu_diff_t2,
             KLD_t1 = KLD_t1,
             KLD_t2 = KLD_t2,
             JSD_t1 = JSD_t1,
             JSD_t2 = JSD_t2)
}

par(mfrow= c(2,2))
with(results, plot(mu_diff_t1, mu_diff_t2)); with(results, cor.test(mu_diff_t1, mu_diff_t2))
with(results, plot(log(KLD_t1), log(KLD_t2))); with(results, cor.test(log(KLD_t1), log(KLD_t2)))
with(results, plot(JSD_t1, JSD_t2)); with(results, cor.test(JSD_t1, JSD_t2))

