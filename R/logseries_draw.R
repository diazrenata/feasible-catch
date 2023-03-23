draw_from_logseries <- function(params, ndraws = 1000) {
  
  draws = replicate(ndraws, one_draw(params), simplify = F)

  all_draws <- dplyr::bind_rows(draws, .id = "draw")  %>%
    mutate(source = "logseries")
  
  all_draws
}

get_parameters <- function(J, v) {
  
  alpha =   2 * v * J
  
  S = ceiling(alpha * log(1 + (J / alpha)))
  
  return(
    tibble::tibble(
      J = J,
      v = v,
      N = J,
      alpha = alpha,
      S = S
    )
  )
}

one_draw <- function(params) {
  
  draw = sads::rls(n = params$S, N = params$N, alpha = params$alpha)
  
  draw = sort(draw)
  
  long_draw =
    tibble::tibble(
      rank = 1:length(draw),
      abund = draw
    )
  
  return(long_draw)
}
