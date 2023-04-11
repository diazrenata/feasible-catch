# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint
library(tidyr)
library(dplyr)

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint


pars <- read.csv(here::here("dense_pars.csv"))


ss <- pars$richness
ns <- pars$abundance

p_table <- readRDS(here::here("ptables", "masterp_tall.Rds"))

vals <- data.frame(ss = ss, ns = ns)

#vals <- vals[1:5, ]

# Replace the target list below with your own:

t1 <- tar_map(vals,
                targets::tar_target(analysis, full_workflow_sn(s = ss, n = ns, p_table = p_table)))

t2 <- tarchetypes::tar_combine(
  all,
  t1,
  command = bind_rows(!!!.x)
)

list(t1, t2)
