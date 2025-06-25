#' Replaces missing (NA) values in given columns of a dataframe.
#' @export
#' @param x Dataframe containing the columns with missing values to be replaced
#' @param cols Vector of column names (strings) to have missing values replaced
#' @param method Method for replacing missing values. Default is lowest value in the column divided by 2
#'
#' @importFrom dplyr mutate_at


replace_missing <- function(x, cols, method = "lod2"){

    if(method == "lod2"){
    val <- mutate_at(x, vars(cols), ~replace(., is_zero_or_na(.), min(.[.>0], na.rm = TRUE)/2))
    }

   if(method == "lod5"){
    val <- mutate_at(x, vars(cols), ~replace(., is_zero_or_na(.), min(.[.>0], na.rm = TRUE)/5))
   }
    return(val)


}
