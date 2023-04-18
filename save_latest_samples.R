targets::tar_load("all")

library(dplyr)

sample_summaries <- all %>%
  group_by(richness, abundance) %>%
  summarize(n = dplyr::n(),
            mean = mean(hill1),
            sd = sd(hill1),
            n_hill1 = length(unique(hill1))) %>%
  ungroup()

write.csv(sample_summaries, "1500_samples.csv", row.names = F)
