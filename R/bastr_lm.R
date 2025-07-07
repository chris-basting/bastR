#' This is a function for performing and summarizing linear models.
#' It is primarily meant to be used for generating many different models
#' with the same fixed/random effects and different response variables, e.g.,
#' for many different metabolites or cytokines. If there are no random
#' effects, this will use the lm() function. If there are random effects, this
#' will use the lmerTest() function.
#'
#'
#'@param x A data frame containing the predictors and response variables
#'@param vars A character vector specifying the names of the response variables to generate linear models for
#'@param fixed_effects A character vector specifying the names of the fixed effects. For example: c("group", "age")
#'@param random_effects A character vector specifying the random effects. For example: c("(1 | PID)")
#'@param check_model A boolean value specifying if linear model assumptions (e.g., linearity of residuals, heteroscedasticity, outliers) should be tested using the performance R package
#'
#'
#'
#'@importFrom lmerTest lmer
#'@importFrom broom.mixed tidy
#'@importFrom dplyr filter mutate bind_rows
#'
#'@export
#'

setClass("bastR_lm",
         slots = list(
           results = "data.frame",
           models = "list"
         ))

bastr_lm <- function(x, vars, fixed_effects, random_effects = NULL, check_model = T){

  # Check that all variables are present in the data frame
  missing_vars <- setdiff(vars, colnames(x))
  if (length(missing_vars) > 0) {
    stop("Missing required columns: ", paste(missing_vars, collapse = ", "))
  }

  model.list <- list()  # store your models
  results <- list() # store each models results

  for (i in c(vars)) {  # loop through predictors

    # Build formula safely using backticks
    formula <- as.formula(paste0("`",i,"`"," ~ ", paste0(c(fixed_effects, random_effects), collapse = " + ")))

    # Fit model using lm if no random effects, or lmerTest if there are
    if(!is.null(random_effects)){
    model <- lmerTest::lmer(formula, data = x)
    } else{
    model <- stats::lm(formula, data = x)
    }
    #Add model to list of models
    model.list[[i]] <- model

    #Tidy the model output
    model_tidy <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
      filter(term != "(Intercept)")  # keep only the predictor terms

    # Save results
    results[[i]] <- model_tidy %>%
      mutate(Variable = i)


  }

  # Combine all results into a dataframe
  result.df <- bind_rows(results)

  S4.return <- new("bastR_lm", results = result.df, models = model.list)

  return(S4.return)

}








