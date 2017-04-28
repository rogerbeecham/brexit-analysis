add_variables <- function(boot_super, cats) {
  for(i in 1:nrow(cats)) {
    add_row <- TRUE
    for(j in 1:nrow(boot_super)) {
      if(cats$row[i]==boot_super$row[j]) {
        add_row <- FALSE
      }
    }
    if(add_row) {
      boot_super <- boot_super %>% 
        tibble::add_row(
          row=cats$row[i],
          observed=NA,
          upper=NA,
          lower=NA,
          superregion=boot_super$superregion[1]
        ) 
    }
  }
  return(boot_super)
}


do_bootstrap_superregion <- function(data, superregion, cats) {
  models <- modelr::bootstrap(data, 1000) %>%
    mutate(
      model = purrr::map(strap,do_lasso_boot),
      coef = purrr::map(model, ~coef(., s="lambda.1se")),
      coef = coef %>% as.matrix %>% purrr::map(broom::tidy)) %>%
        unnest(coef) %>%
          group_by(row) %>%
            summarise(
              lower=quantile(value,0.05, na.rm = TRUE),
              upper=quantile(value,0.95, na.rm = TRUE)) %>%
                filter(row!="(Intercept)")
                cv_fit_lasso <- glmnet::cv.glmnet(as.matrix(data %>% select(younger_adults:eu_born)),  as.matrix(data %>% select(Leave)), alpha=1)
  observed <- as_tibble(tidy(glmnet::coef(cv_fit_lasso, s = "lambda.1se"))) %>%
                mutate(observed=value) %>%
                  select(row, observed) %>%
                    filter(row!="(Intercept)")
  models <- left_join(models,observed) %>%
              mutate(superregion = rep(superregion,nrow(models))) %>%
              add_variables(.,cats)
  return(models)
}
