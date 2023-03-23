compare_wilcox <- function(vect1, vect2) {
  
 wt=  wilcox.test(vect1, vect2, alternative = "two.sided", conf.int = T)
  
 
 p = wt$p.value
loc_diff =wt$estimate
 loc_confint = (wt$conf.int)
 
 return(data.frame(
   p = p,
   loc_diff = loc_diff[1],
   diff_lower = loc_confint[1],
   diff_upper = loc_confint[2]
 ))
 
 
}
