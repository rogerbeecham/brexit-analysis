# Group means -- LAs nested within regions.
lmer_group <- function(data) {
  model <- data %>% 
    lmer(Leave ~ (1|Region), data = ., REML = FALSE)
  return(model)
}
# Varying-intercept, fixed effect.
lmer_intercept <- function(data, x) {
  model <- data %>% 
    select_(lazyeval::interp(~matches("^Leave$")),
            lazyeval::interp(~matches("^Region$")),
            x=lazyeval::interp(~matches(paste0("^", eval(x), "$"))))  %>%
    lmer(Leave ~ x + (1 | Region), data = ., REML = FALSE)
  return(model)
}
# Varying-intercept and slope. 
lmer_slope <- function(data, x) {
  print(x)
  model <- data %>% 
    select_(lazyeval::interp(~matches("^Leave$")),
            lazyeval::interp(~matches("^Region$")),
            x=lazyeval::interp(~matches(paste0("^", eval(x), "$"))))  %>%
    lmer(Leave ~ x + (1 + x | Region), data = ., REML = FALSE)
  return(model)
}
# Fetches summary statistics for multi-level models.
get_randoms_fitted <- function(intercept, slope, data, x){
  fits_intercept <- sem.model.fits(list(intercept), data) 
  fits_slope <- sem.model.fits(list(slope), data) 
  likelihood <- anova(intercept, slope)$`Pr(>Chisq)`[2]
  return(
    data_frame(
      var_name=x,
      conditional_intercept=fits_intercept$Conditional,
      marginal_intercept=fits_intercept$Marginal,
      conditional_slope=fits_slope$Conditional,
      marginal_slope=fits_slope$Marginal,
      likelihood=likelihood,
      corr=as.data.frame(VarCorr(slope)) %>% filter(grp=="Region", var2=="x")  %>% .$sdcor,
      fitted_slope=fitted(slope),
      fitted_intercept=fitted(intercept),
      Leave=data$Leave,
      Region=data$Region,
      var_values=data[,x]
    )
  )
}
get_randoms <- function(intercept, slope, data, x){
  fits_intercept <- sem.model.fits(list(intercept), data) 
  fits_slope <- sem.model.fits(list(slope), data) 
  likelihood <- anova(intercept, slope)$`Pr(>Chisq)`[2]
  
  summary <- inner_join(get_summary(intercept, "intercept", x), get_summary(slope, "slope", x)) %>%
    mutate(conditional_intercept=fits_intercept$Conditional,
           marginal_intercept=fits_intercept$Marginal,
           conditional_slope=fits_slope$Conditional,
           marginal_slope=fits_slope$Marginal,
           likelihood=likelihood,
           corr=as.data.frame(VarCorr(slope)) %>% filter(grp=="Region", var2=="x")  %>% .$sdcor
    )
  return(summary)
}
# Fetches level 2 residuals and CIs for random intecept model.
library(rlang)
get_summary <- function(model, type, x) {
  u0 <- ranef(model, condVar = TRUE)
  u0$Region$x <- NULL
  u0se <- ifelse(type=="intercept", sqrt(attr(u0[[1]], "postVar")[1, , ]), sqrt(attr(u0[[1]], "postVar")[1, , ])[1,])
  u0se <-  ifelse(type=="intercept", sqrt(attr(u0[[1]], "postVar")[1, , ]), sqrt(attr(u0[[1]], "postVar")[1, , ])[1,])
  id <- rownames(u0[[1]])
  tab <- cbind(id, u0[[1]], u0se)
  colnames(tab) <- c("id", "u0", "u0se")
  tab <- tab %>% mutate(upper = u0+(1.96*u0se),
                        lower = u0-(1.96*u0se),
                        x_var=x) %>%
    setNames(c(names(.)[1], paste0(names(.)[-1],paste0("_",type)))) 
}
get_variances <- function(data, x) {
  if(is.null(x)) {
    model <- lmer_null(data)
  } else {
    model <- lmer_intercept(data, x)
  }
  between <- as.data.frame(VarCorr(model)) %>% filter(grp=="Region") %>% .$vcov
  within <- as.data.frame(VarCorr(model)) %>% filter(grp=="Residual") %>% .$vcov
  p_region <- between/(between+within)
  return(
    data_frame(
      type=ifelse(is.null(x),"null",x),
      within=within,
      between=between,
      total=within+between,
      p_region=p_region)
  )
}