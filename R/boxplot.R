#' Creates a boxplot in the style I like using ggplot
#' Input is a dataframe that contains both the numeric data and categories/groupings
#' @export
#' @param x A dataframe
#' @param var A numeric variable in x to be plotted on y axis
#' @param group A categorical variable in x to be plotted on x axis
#'

ggplot(data = test, aes(x=Species, y = Sepal.Length)) +
       geom_boxplot(outlier.shape = NA, aes(color = Species, fill = Species)) +
       geom_jitter(width = .2) +
       theme_prism() +
  scale_colour_prism(palette = "floral") +
  scale_fill_prism(palette = "floral")
