summarize_draws <- function(long_draws) {
  
  hill1  <- long_draws %>%
    select(-source) %>%
    tidyr::pivot_wider(id_cols = draw, names_from = rank, values_from = abund, values_fill = 0) %>%
    select(-draw) %>%
    hillR::hill_taxa(q = 1) 
  
  hill2 <- long_draws %>%
    select(-source) %>%
    tidyr::pivot_wider(id_cols = draw, names_from = rank, values_from = abund, values_fill = 0) %>%
    select(-draw) %>%
    hillR::hill_taxa(q = 2)
  
  draw_summary <- long_draws %>%
    group_by(draw, source) %>%
    summarize(richness = dplyr::n(),
           abundance = sum(abund),
           even = vegan::diversity(abund, index = "simpson")) %>%
    ungroup() %>%
    arrange(as.numeric(draw)) %>% 
    mutate(hill1 = hill1,
           hill2=hill2)
  
  draw_summary
  
}