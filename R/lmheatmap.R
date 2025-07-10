#' This function is for creating a mixed plot for visualizing many multiple
#' linear regression estimates for fixed effects. Creates line plots for
#' the primary fixed effect of interest and heatmaps for the estimates of
#' covariates
#'
#' @param x Data frame with the results from bastr_lm, or in the format of the output from broom_mixed of linear models
#' @param predictor Character string. Primary predictor to be used for showing line plot estimates
#'
#' @export
#'
#'


lmheatmap <- function(){

  #step 1: filter results to just have main predictor variable
  lineplot.df <- dplyr::filter(x, grepl(predictor, term) | term == predictor)


  ggplot(lineplot.df, aes(x = Variable, y = estimate)) +
    geom_point(size = 3, color = "steelblue") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "gray40") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    #geom_vline(xintercept = group_breaks[-1] + 0.5, linetype = "dashed", color = "black", linewidth = .2) +
    #geom_text(aes(label = plabel, y = conf.high + 0.1), size = 4, color = "black") +
    coord_flip() +  # flips x/y axes for easier reading
    labs(title = "Linear Model Coefficients with 95% Confidence Intervals",
         x = "Term", y = "β estimate") +
    theme_linedraw() +
    facet_wrap(~ term, nrow = 1) +
    theme(
      strip.background = element_rect(fill = "lightgray", color = "black"),
      strip.text = element_text(face = "bold", size = 12, color = "black")
    )



}
