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
#' @import ggplot2
#' @importFrom rstatix get_y_position
#' @importFrom ggpubr stat_pvalue_manual
#' @importFrom dplyr left_join
#'
#'


#library(ggplot2)
#library(rstatix)
#library(ggprism)
#library(dplyr)
#library(ggpubr)


bastr_boxplot <- function(x, var, group, title = NULL, stats = NULL, save.plot = F, path = getwd(), group.colors = NULL){
p <- ggplot(data = x, aes(x=group, y=var)) +
       geom_boxplot(outlier.shape = NA, aes(color = group, fill = group)) +
       geom_jitter(width = .2) +
       theme_prism() +
       ggtitle(title)

# If title is provided, add labels
#if (!is.null(title)) {
#  p <- p + ggtitle(title)
#  }

# If colors aren't provided use default, otherwise use specified group colors
if (is.null(group.colors)) {
    p <- p + scale_colour_prism(palette = "colors") +
             scale_fill_prism(palette = "colors")

  } else(
   p <- p
)


# If stats are provided, add p values
if (!is.null(stats)) {

  positions <- get_y_position(x, formula = var ~ group) %>%
    right_join(stats, by = c("group1", "group2"))
  p <- p + ggtitle(title)
  }

}





