#' Extract relevant parameters from a fitted Lavaan model.
#' 
#' Internal function to extract parameters of interest from a fitted model.
#' 
#' @param modfit a fitted model object
getEstimates <- function(modfit, f = 2){
  load <- lavInspect(object = modfit, 
                     what = "std")$lambda %>% 
    as_tibble %>% 
    rownames_to_column(var = "label") %>% 
    rowwise() %>% 
    transmute(label = paste0("b", label),
              est = ifelse(f==2, sum(f1, f2), f1))
  
  s1 <- tibble(label = "s1",
               est = lavInspect(object = modfit, 
                                what = "std")$psi[2,1])
  
  par_est <- bind_rows(load, s1)
  
  se_load <- lavInspect(object = modfit, 
                        what = "se")$lambda %>% 
    as_tibble %>% 
    rownames_to_column(var = "label") %>% 
    rowwise() %>% 
    transmute(label = paste0("b", label),
              se = sum(f1, f2))
  
  se_s1 <- tibble(label = "s1",
                  se = lavInspect(object = modfit, 
                                  what = "se")$psi[2,1])
  
  
  se <- bind_rows(se_load, se_s1)
  
  left_join(par_est, se, by = "label")
}