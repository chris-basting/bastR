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
#' @import ggprism
#' @importFrom rstatix get_y_position
#' @importFrom ggpubr stat_pvalue_manual
#' @importFrom dplyr right_join
#'
#'


bastr_boxplot <- function(x, var, group, title = NULL, stats = NULL, save.plot = F, path = getwd(), group.colors = NULL, pval_stars = F){
p <- ggplot(data = x, aes(x=!!sym(group), y=!!sym(var))) +
       geom_boxplot(outlier.shape = NA, aes(fill = !!sym(group))) +
       geom_jitter(width = .2) +
       theme_prism() +
       ggtitle(title)

# If colors aren't provided use default, otherwise use specified group colors
if (is.null(group.colors)) {
    p <- p + scale_fill_prism(palette = "colors")

  } else{
   p <- p + scale_fill_manual(values = group.colors)
  }

# If stats are provided, add p values
if (!is.null(stats)) {

  positions <- get_y_position(x, formula = as.formula(paste0(var, " ~ ", group))) %>%
    right_join(stats, by = c("group1", "group2")) %>%
    rstatix::add_significance(p.col = "p.value", output.col = "label")

  if(pval_stars == T){
  p <- p + stat_pvalue_manual(data = positions, label = "label")
  } else{
    p <- p + stat_pvalue_manual(data = positions, label = "p.value")
  }

  p

}

if (save.plot == T){
  ggsave(filename = paste0(group,"_",var,"_boxplot.pdf"), plot = p, path = path)
}

return(p)

}








