library(ggplot2)
library(dplyr)
library(cowplot)

# read data

sim.diff <- read.csv("similarity distributions/different sequences/distance.csv") %>% 
  mutate(sequences = "different")
sim.same1 <- read.csv("similarity distributions/same sequences/sequence 1/distance.csv") %>% 
  mutate(sequences = "same")
sim.same2 <- read.csv("similarity distributions/same sequences/sequence 2/distance.csv") %>% 
  mutate(sequences = "same")
sim.same <- rbind(sim.same1, sim.same2) %>%
            filter(distance > 0)

sim <- rbind(sim.diff, sim.same)

#---------------------------------- similarity

# Basic histogram

sim.diff.g <- ggplot(sim.diff, aes(x=distance)) +
  geom_histogram(color="black", fill="pink", binwidth = 1) + 
  scale_x_continuous("\nLD distance - different sequences") +
  scale_y_continuous("Frequency\n") +
  theme_classic()

sim.same.g <- ggplot(sim.same, aes(x=distance)) +
  geom_histogram(color="black", fill="lightblue", binwidth = 1) + 
  scale_x_continuous("\nLD distance - same sequences") +
  scale_y_continuous("Frequency\n") +
  theme_classic()

sim.overall.g <- ggplot(sim, aes(x=distance, colour = sequences, fill = sequences)) +
  scale_color_manual(values=c("indianred", "mediumpurple1")) +
  scale_fill_manual(values=c("indianred", "mediumpurple1")) +
  geom_histogram(alpha=0.5, position="identity") + 
  scale_x_continuous("\nLD distance", limits = c(200, 450), expand = c(0,0)) +
  scale_y_continuous("Frequency\n") +
  theme_classic()

sim.overall.g 

ggsave("plots/sim.overall.g.png", dpi = 800, height = 5, width = 7, bg = "white")

#---------------------------------- similarity after random allocation

sim.diff.random.allo <- read.csv("similarity distributions/different sequences/distance.random.allo.csv") %>% 
                        mutate(sequences = "different")
sim.same1.random.allo <- read.csv("similarity distributions/same sequences/sequence 1/distance.random.allo.csv") %>% 
                        mutate(sequences = "same")
sim.same2.random.allo <- read.csv("similarity distributions/same sequences/sequence 2/distance.random.allo.csv") %>% 
                        mutate(sequences = "same")
sim.same.random.allo <- rbind(sim.same1.random.allo, sim.same2.random.allo) %>% filter(distance > 0)

sim.random.allo <- rbind(sim.diff.random.allo, sim.same.random.allo)

hist(sim.diff.random.allo$distance, xlab = "LD distance - different sequence", col = "pink")

hist(sim.same.random.allo$distance, xlab = "LD distance - same sequence", col = "lightblue")

# Basic histogram

sim.diff.g.random.allo <- ggplot(sim.diff.random.allo, aes(x=distance)) +
  geom_histogram(color="black", fill="pink", binwidth = 1) + 
  scale_x_continuous("\nLD distance - different sequences") +
  scale_y_continuous("Frequency\n") +
  ylim(0, 80) +
  theme_classic()
sim.diff.g.random.allo

sim.same.g.random.allo <- ggplot(sim.same.random.allo, aes(x=distance)) +
  geom_histogram(color="black", fill="lightblue", binwidth = 1) + 
  scale_x_continuous("\nLD distance - same sequences") +
  scale_y_continuous("Frequency\n") +
  ylim(0, 80) +
  theme_classic()
sim.same.g.random.allo

sim.overall.g.random.allo <- ggplot(sim.random.allo, aes(x=distance, colour = sequences, fill = sequences)) +
  scale_color_manual(values=c("indianred", "mediumpurple1")) +
  scale_fill_manual(values=c("indianred", "mediumpurple1")) +
  geom_histogram(alpha=0.5, position="identity") + 
  scale_x_continuous("\nLD distance", limits = c(200, 450), expand = c(0,0)) +
  scale_y_continuous("Frequency\n", limits = c(0, 450), expand = c(0, 0)) +
  theme_classic()

sim.overall.g.random.allo

ggsave("plots/sim.overall.g.random.allo.png", dpi = 800, height = 5, width = 7, bg = "white")

#--------------------------------------------------- Similarity for study 1 after random allocation

sim.study1 <- read.csv("similarity distributions/study 1/Data.study1.csv") %>% 
              group_by(Participant) %>% 
              summarise(sequences = sequences[1], Distance = Distance[1])

# Basic histogram

sim.overall.g.study1 <- ggplot(sim.study1, aes(x=Distance, colour = sequences, fill = sequences)) +
  scale_color_manual(values=c("#0072B2", "aquamarine2")) +
  scale_fill_manual(values=c("#0072B2", "aquamarine2")) +
  geom_histogram(alpha=0.5, position="identity") + 
  scale_x_continuous("\nLD distance", limits = c(200, 450), expand = c(0,0)) +
  scale_y_continuous("Frequency\n", expand = c(0, 0)) +
  theme_classic()

sim.overall.g.study1

ggsave("plots/sim.overall.g.study1.png", dpi = 800, height = 5, width = 7, bg = "white")


#--------------------- merge all plots

grid.sim <- plot_grid(sim.overall.g,  sim.overall.g.random.allo, sim.overall.g.study1, ncol = 1, labels = c("A","B", "C"))

ggsave("plots/sim.grid.png", dpi = 800, height = 10, width = 7, bg = "white")


