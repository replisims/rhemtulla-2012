# Figures to reproduce
# - remove heywood cases and non-convergent cases
# remove outliers i.e. cases that produce a standard error greater than 1
- remove when cat-ls produced a factor loading >1 or ML produced standardized factor loading >1

#relative bias
rel_bias <- function(est_param, true_param) <- (mean(est_param) - true_param)/true_param


# Table 1 
- generate samples of size 1000000 for each condition and record the skew and kurtosis of the observed distributions

#fig 3
- only lambda = 0.3 and 0.7 and only sample size 100 and 600 normal
averaged across model size and loadings with identical true parameter value
# fig 4
- only lambda = 0.3 and 0.7 and only sample size 100 and 600 non-normal
averaged across model size and loadings with identical true parameter value

#fig 5 
- parameter estimate for factor correlation averaged across model size
sample size 100 and 600 only

#fig 6
- coverage
coverage is defined as the proportion of 95% CI created using robust standard 
error estimation around the estimated parameter value tha include the true parameter value


# figure 7
same as 6 but with non-normal

# Figure 9
type 1 error rate_backoff(emporical type 1 error at alpha = .05 is 
defined as the proportion of converged replications that generate a p value less than .05 )