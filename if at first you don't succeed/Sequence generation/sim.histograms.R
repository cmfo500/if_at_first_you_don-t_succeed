library(ggplot2)
library(dplyr)
library(cowplot)

sim.diff <- read.csv("similarity distributions/different sequences/distance.csv") %>% 
  mutate(sequences = "different")
sim.same1 <- read.csv("similarity distributions/same sequences/sequence 1/distance.csv") %>% 
  mutate(sequences = "same")
sim.same2 <- read.csv("similarity distributions/same sequences/sequence 2/distance.csv") %>% 
  mutate(sequences = "same")
sim.same <- rbind(sim.same1, sim.same2) %>%
            filter(distance > 0)

sim <- rbind(sim.diff, sim.same)

hist(sim.diff$distance, xlab = "LD distance - different sequence", col = "pink")

hist(sim.same$distance, xlab = "LD distance - same sequence", col = "lightblue")

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
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  geom_histogram(alpha=0.5, position="identity") + 
  scale_x_continuous("\nLD distance") +
  scale_y_continuous("Frequency\n") +
  theme_classic()

sim.overall.g <- ggplot(sim, aes(x=distance, colour = sequences, fill = sequences)) +
  scale_color_manual(values=c("indianred", "mediumpurple1")) +
  scale_fill_manual(values=c("indianred", "mediumpurple1")) +
  geom_histogram(alpha=0.5, position="identity") + 
  scale_x_continuous("\nLD distance", limits = c(200, 450), expand = c(0,0)) +
  scale_y_continuous("Frequency\n", expand = c(0, 0)) +
  theme_classic()

sim.overall.g 

ggsave("plots/sim.overall.g.png", dpi = 800, height = 5, width = 7, bg = "white")

# random allocation

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

sim.same.g.random.allo <- ggplot(sim.same.random.allo, aes(x=distance)) +
  geom_histogram(color="black", fill="lightblue", binwidth = 1) + 
  scale_x_continuous("\nLD distance - same sequences") +
  scale_y_continuous("Frequency\n") +
  ylim(0, 80) +
  theme_classic()

sim.overall.g.random.allo <- ggplot(sim.random.allo, aes(x=distance, colour = sequences, fill = sequences)) +
  scale_color_manual(values=c("indianred", "mediumpurple1")) +
  scale_fill_manual(values=c("indianred", "mediumpurple1")) +
  geom_histogram(alpha=0.5, position="identity") + 
  scale_x_continuous("\nLD distance", limits = c(200, 450), expand = c(0,0)) +
  scale_y_continuous("Frequency\n", expand = c(0, 0)) +
  theme_classic()

sim.overall.g.random.allo

ggsave("plots/sim.overall.g.random.allo.png", dpi = 800, height = 5, width = 7, bg = "white")

grid.sim <- plot_grid(sim.overall.g,  sim.overall.g.random.allo, ncol = 1, labels = c("A","B"))

ggsave("plots/sim.grid.png", dpi = 800, height = 8, width = 7, bg = "white")

