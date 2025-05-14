#' Creates a boxplot in the style I like using ggplot
#' @export
#' @param x (required) A dataframe containing the values to be plotted
#' @param var (required) The numeric variable to be plotted on y axis
#' @param group (required) The categorical variable to be plotted on x axis
#' @param path Filepath for generated plots, default is working directory
#' @param title Title for plot
#' @param stats Data frame containing contrasts and their p-values
#' @param saveplot True/False as to whether the plot should be saved to path
#'
#' @importFrom ggplot2 ggplot
#' @importFrom rstatix get_y_position
#' @importFrom ggpubr stat_pvalue_manual
#' @importFrom dplyr left_join
#'
#'


library(ggplot2)
library(rstatix)
library(ggprism)
library(dplyr)
library(ggpubr)


bastr_boxplot <- function(x, var, group, title = NULL, stats = NULL, path = getwd()){
p <- ggplot(data = x, aes(x=group, y=var)) +
       geom_boxplot(outlier.shape = NA, aes(color = group, fill = group)) +
       geom_jitter(width = .2) +
       theme_prism() +
  scale_colour_prism(palette = "floral") +
  scale_fill_prism(palette = "floral") +
  stat_pvalue_manual(data = positions)

# If title is provided, add labels
if (!is.null(title)) {
  p <- p + ggtitle(title)
}

}


test <- data.frame(contrast= c("setosa - versicolor", "setosa - virginica", "versicolor - virginica"),
                   group1 = c("setosa", "setosa", "versicolor"),
                   group2 = c("versicolor", "virginica", "virginica"),
                   p.adj = c(0.05, 0.01, 0.1))

positions <- get_y_position(iris, formula = Sepal.Length ~ Species) %>%
  right_join(test, by = c("group1", "group2"))
