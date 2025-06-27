#' This is a function for performing and summarizing linear models.
#' It is primarily meant to be used for performing many different models
#' with the same fixed/random effects and different response variables, i.e.,
#' for many different metabolites or cytokines.
#'
#'@param x A data frame containing the predictors and response variables
#'@param vars A character vector specifying the names of the response variables to generate linear models for
#'@param formula A character value specifying the formula of predictors to use for each response variable. Should include all fixed and random effects. Example: "age + bmi + (1|PID)"
#'@param check_model A boolean value specifying if linear model assumptions (e.g., linearity of residuals, heteroscedasticity, outliers) should be tested using the performance R package
#'
#'
#'
#'@importFrom lmerTest lmer
#'@importFrom broom tidy
#'
#'@export
#'

setClass("MyObject",
         slots = list(
           results = "data.frame",
           models = "list"
         ))

bastr_lm <- function(x, vars, formula, check_model = T){


  model.list <- list()  # store your models
  results <- list() # store each models results

  for (i in c(vars)) {  # loop through predictors

    # Build formula safely using backticks
    formula <- as.formula(paste0("`",i,"`"," ~ ", formula))

    # Fit model
    model <- lmerTest::lmer(formula, data = x)

    #Add model to list of models
    model.list[[var]] <- model

    #Tidy the model output
    model_tidy <- broom::tidy(model) %>%
      filter(term != "(Intercept)")  # keep only the predictor terms

    # Get R-squared
    r_squared <- summary(model)$r.squared

    # Save results
    results[[var]] <- model_tidy %>%
      mutate(R2 = r_squared,
             Variable = var)


  }

  # Combine all results into a dataframe
  result.df <- bind_rows(results)

  S4.return <- new("MyObject", results = result.df, models = model.list)

  return(S4.return)

}

