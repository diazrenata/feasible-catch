draw_from_feasible_set <- function(params, ndraws = 1000, p_table = NULL) {
  
  fs_draws <- feasiblesads::sample_fs(round(params$S), params$N, nsamples = ndraws, p_table = p_table) %>%
    t() %>%
    as.data.frame() %>%
    mutate(rank = dplyr::row_number()) %>%
    tidyr::pivot_longer(-rank, names_to = "draw", values_to = "abund") %>%
    mutate(draw = stringr::str_remove(draw, "V")) %>%
    mutate(draw = as.numeric(draw),
           source = "feasible")
  
  return(fs_draws)
  
}