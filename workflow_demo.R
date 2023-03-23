source_files <- list.files(here::here("R"), full.names = T)
for(file in source_files) {
  source(file)
}
library(dplyr)
library(ggplot2)


p_table <- readRDS(here::here("ptables", "masterp_tall.Rds"))


toy_J = 50
toy_v = .011

demo <- full_workflow(toy_J, toy_v, p_table = p_table)
