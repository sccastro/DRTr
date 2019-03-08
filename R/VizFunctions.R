#' Vizualize data .csv Files
#'
#' Use these functions to vizualize Detection Response Task outcome variables according to your subjects, conditions, etc. See Examples for more.
#'
#' @param dataname The data.frame object you're going to vizualize
#' @param plottype Specify the type of plot you'd like to produce
#' @param subid The name of the column for your Subject ID numbers.
#' @param cond The name of the column for your condition.
#' @param DV Dependent Variable of either HR (Hit Rate) or RT (Reatction Time)
#' @examples
#'
#' dat <- combineCSV(fpath = getwd(), dataname = newdf)
#'
#'@importFrom magrittr %>%
#'@import dplyr
#'@import ggplot2
#'@importFrom stats na.omit
#'@export


vizBySubject <- function(dataname,plottype, subid,cond, DV) {
  if (plottype == "bar" & DV == "rt") {
  dataname %>% #Get your data from Global Environment
    group_by(subid,cond) %>% #group by subid and condition
    na.omit() %>% #Clear NA rows
    summarize(Mean = mean(rt, na.rm = T)) %>% #Get means
    ggplot(aes(x = subid, y = Mean, group = cond)) +
      geom_col(aes(fill = cond)) +
      theme_classic()#Plot
  } else {
    dataname %>% #Get your data from Global Environment
      group_by(subid,cond) %>% #group by subid and condition
      na.omit() %>% #Clear NA rows
      summarize(DV = mean(DV)) %>% #Get means
      ggplot(aes(x = subid, y = DV, group = cond)) +
      geom_point(aes(color = cond)) +
      theme_classic()#Plot
  }

}



# vizByCondition


