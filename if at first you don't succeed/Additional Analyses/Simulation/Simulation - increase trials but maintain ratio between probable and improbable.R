# Simulation - increase the number of improbable trials
# Manuscript title: "Reliability of the Serial Reaction Time task: If at first you don't succeed, try try try again"
# Author: "Cátia Margarida Oliveira, Marianna E. Hayiou-Thomas, Lisa Henderson; University of York"

library(stats)
library(tidyr)
library(ggplot2)
library(dplyr)

set.seed(15)
num_imp <- seq(200, 3000, 200)
names <- c('200', '400', '600', '800', '1000', '1200', '1400', '1600', '1800', '2000', '2200', '2400', '2600', '2800', '3000')

diff1 <- as.data.frame(matrix(nrow=1000,ncol=length(num_imp)))
diff2 <- as.data.frame(matrix(nrow=1000,ncol=length(num_imp)))
colnames(diff1) <- names
colnames(diff2) <- names

results1 <- list()
results2 <- list()

for (x in 1:500){ #500 estimations of reliability per parameters
  for (j in 1:150) { #150 participants per reliability estimation
    for (i in 1:length(num_imp)){
      sample <- num_imp[i]
      m = rnorm(1, mean=-1.2,.2)
      s = runif(1)
      
      data11 = rlnorm(sample*.9,m,s)
      data12 = rlnorm(sample*.1,m+.5,s)
      
      data21 = rlnorm(sample*.9,m,s)
      data22 = rlnorm(sample*.1,m+.5,s)
      
      diff1[j,i] = mean(data12)-mean(data11)
      diff2[j,i] = mean(data22)-mean(data21)
    }
  }
  results1[[x]] <- diff1
  results2[[x]] <- diff2
} 

data <- as.data.frame(matrix(nrow=100,ncol=length(num_imp)))
colnames(data) <- names

for (y in 1:100){
  for (i in 1:length(names)){
    num <- names[i]
    data[y, i] <- cor.test(results1[[y]][[as.character(num)]], results2[[y]][[as.character(num)]])$estimate
  }
}

data_long <- gather(data, trial_no, corr, '200':'3000', factor_key = TRUE)

reg<-lm(corr ~ trial_no, data = data_long)

summary(reg)

means <- data_long %>% group_by(trial_no) %>% summarise(corr = mean(corr))

# Plot correlations
png("plots/simulation_plot_ratio90-10.png", width = 750)

ggplot(data_long, aes(x = trial_no, y = corr, color = trial_no)) +  theme_bw() + geom_point(alpha = .4) +  geom_point(data = means, size = 4) + 
  xlab("\nN. of trials: 90% - 10% ratio") +  ylab("Test-retest reliability\n") + theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
 panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_hline(yintercept=.70, linetype="dashed", color = "black") +  ylim(0, 1)

dev.off()
