
full_workflow <- function(J, v, p_table = NULL) {
  
  toy_params <- get_parameters(J, v)
  
  fs_draws <- draw_from_feasible_set(toy_params, p_table = p_table)
  
  fs_summary <- summarize_draws(fs_draws)
  
  
  return(fs_summary)
  
}



full_workflow_sn <- function(s, n, p_table = NULL) {
  
  toy_params <- get_parameters_sn(s, n)
  
  fs_draws <- draw_from_feasible_set(toy_params, p_table = p_table)
  
  fs_summary <- summarize_draws(fs_draws)
  
  
  return(fs_summary)
  
}

get_parameters_sn = function(s, n) {

  return(list(
    S = s,
    N = n
  ))
    
}

