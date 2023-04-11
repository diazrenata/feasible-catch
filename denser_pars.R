library(dplyr)
library(ggplot2)
theme_set(theme_bw())

easy_pars <- read.csv(here::here("easy_pars.csv"))

easy_pars <- filter(easy_pars, S < 200, S > 2)



ggplot(easy_pars, aes(S, J)) + 
  geom_point() +
  geom_ribbon(aes(x = seq(0, 200, length.out = 517), ymin = 0, ymax = 40700), inherit.aes = F, alpha = .1, fill = "pink")

# Yeah see this is weird.

#p_table <- readRDS(here::here("ptables", "masterp_tall.Rds"))

# One option would be to sample literally at random in this space...

set.seed(1988)

random_richness = sample.int(200, size = 500, replace = TRUE)

random_abundance = sample.int(20000, size = 500, replace = TRUE)

new_pars <- data.frame(
  richness = random_richness,
  abundance = random_abundance
)

new_pars <- new_pars %>%
  filter(richness + 1 < abundance,
         richness > 2 ) %>%
  distinct()


ggplot(easy_pars, aes(S, J)) + 
  geom_point() +
  geom_point(data = new_pars, aes(richness, abundance), color = "green") +
  geom_ribbon(aes(x = seq(0, 200, length.out = 517), ymin = 0, ymax = 40700), inherit.aes = F, alpha = .1, fill = "pink")

write.csv(new_pars, "dense_pars.csv", row.names = F)

