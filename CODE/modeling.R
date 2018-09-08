##################
#### Loadings ####
options(na.action = "na.exclude")

# Load convenience functions,
library(here)
here = here::here
library(glue)
library(pryr)
## For common R-specific tasks
source(glue("{here()}/CODE/convenience.R"))

library(tibbletime)
library(imputeTS)
library(forecast)
library(xts)
library(rlang)
library(GGally)
library(ggfortify)
library(ggrepel)
library(lme4)

# Load canonical set
try(FORCE_FRESH_FETCH %<c-% F, silent = T)
try(BUILD_CANON %<c-% F, silent = T)
try(TRAIN_MODEL %<c-% F, silent = T)
try(LOAD_MODEL %<c-% T, silent = T)
##################

###############################
#### Convenience Functions ####
l2 <- function(x, y = NULL) {
  y <- y %||% 0
  ((x - y)^2) %>% sum(na.rm = T) %>% sqrt()
}

l2.normalized <- function(x, y = NULL) {
  y <- y %||% 0
  ((x - y)^2) %>% {sum(., na.rm = T)/sum(isnt.na(.))} %>% sqrt()
}
###############################

#######################
#### EDA & Getters ####

plot_univariate <- function(design_matrix, column = spread) {
  column <- enquo(column)
  xlab <- expr_text(column) %>% str_replace0("~")
  title <-glue("Univariate Display of {xlab}")

  design_matrix %>%
    drop_na(!! column) %>%
    pull(!! column) %>%
    gghistogram(add.normal = T) +
    labs(title = title, x = xlab)
}

plot_univariate_matrix <- function(design_matrix,
                                   columns) {
  title <-glue("Univariate Displays")
  columns %>%
    map(~ plot_univariate(design_matrix, .)) %>%
    ggmatrix(nrow = length(columns), ncol = 1, yAxisLabels = columns, title)
}

plot_bivariate <- function(design_matrix,
                           xvar,
                           yvar) {
  xvar <- enquo(xvar)
  xlab <- expr_text(xvar) %>% str_replace0("~")
  yvar <- enquo(yvar)
  ylab <- expr_text(yvar) %>% str_replace0("~")

  title <-glue("Bivariate Display of {xlab} and {ylab}")

  design_matrix %>%
    select(x = !! xvar, y = !! yvar, quarter, q) %>%
    ggplot(aes(x, y)) +
    geom_jitter() +
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab) +
    theme(legend.position = "bottom")
}
#######################

##################################
#### Feature Tuning & Testing ####

##################################

####################
#### Imputation ####

####################

######################
#### Post-processing ####

######################

#####################
#### Time Series ####

ts.autocorr <- function(design_matrix,
                        column,
                        na_action = na.exclude) {
  column <- enexpr(column)
  design_matrix %>%
    pull(!! column) %>%
    forecast::Acf(na.action = na_action)
} ## TODO: Rename series to expr_text(column)

ts.autocorr.test <- function(design_matrix,
                             column,
                             na_action = na.exclude) {
  column <- enexpr(column)
  design_matrix %>%
    pull(!! column) %>%
    ar(na.action = na_action)
}
#####################

##################
#### Modeling ####
model.train <- function(canon,
                        column = training,
                        formula = spread ~
                          previous_years_revenue + last_known_revenue + `log(Gtrends1)`  +
                          days_until_expected_filing_date * negative_momentum +
                          (1 + days_until_expected_filing_date * negative_momentum |
                             ticker / year / q)) {
  column <- enexpr(column)
  lmer(data = canon %>% tidy_design_matrices(!! column),
       formula = formula, na.action = na.exclude, verbose = 2, REML = T,
       control = lmerControl(calc.derivs = FALSE))
}

model.train <- function(canon,
                        column = training,
                        formula = spread ~
                          previous_years_revenue + last_known_revenue + `log(Gtrends1)` +
                          q + days_until_expected_filing_date * negative_momentum +
                          (1 +
                             q + days_until_expected_filing_date * negative_momentum |
                             ticker / year)) {
  column <- enexpr(column)
  lmer(data = canon %>% tidy_design_matrices(!! column),
       formula = formula, na.action = na.exclude, verbose = 2, REML = T,
       control = lmerControl(calc.derivs = FALSE))
}

model.lm_simple <- function(design_matrix, yvar = spread, xvar = days_until_expected_filing_date, print_summary = T) {
  yvar <- enexpr(yvar)
  xvar <- enexpr(xvar)
  formula <- eval_bare(expr(!! yvar ~ !! xvar))

  bare_model <- expr(lm(!! formula, data = design_matrix))

  model <- eval_tidy(bare_model)

  if (print_summary) summary(model) %>% print()
  return(invisible(model))
}

model.lm_simple_matrix <- function(design_matrix,
                                   columns = exprs(date, year, q,
                                                   days_until_expected_filing_date, negative_momentum,
                                                   last_known_revenue, previous_years_revenue,
                                                   `log(Gtrends1)`, `log(AlexaUniqueVisits)`, `log(Twitter)`)) {
  columns %>%
    set_names() %>%
    map_dfr(~ partial(model.lm_simple_spread, design_matrix, print_summary = F)(!! .) %>% glance(), .id = "x") %>%
    mutate(y = "spread") %>%
    select(x, y, everything()) %>%
    arrange(desc(r.squared))
}

model.predict <- function(design_matrix, model, filter_expression = "T", drop.na = F) {
  predictions <-
    design_matrix %>%
    filter(!! parse_expr(filter_expression)) %>%
    mutate(predicted_spread = predict(model, .))
  if (drop.na) predictions %<>% drop_na(predicted_spread)
  predictions
}

##################
