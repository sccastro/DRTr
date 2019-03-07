# Joel’s awesome function -------------------------------------------------
#' Combine .csv Files
#'
#' Use this function to read in a series of .csv files and transform them into one data.frame. Make sure to save the output to a data.frame. See Examples for more.
#'
#' @param fpath The direcotry that contains all of the .csv files to be combined.
#' @param dataname The name you would like the combined .csv output data.frame to have.
#'
#' @examples
#'
#' dat <- combineCSV(fpath = getwd(), dataname = newdf)
#'
#'@importFrom magrittr %>%
#'@import dplyr
#'@importFrom stringr str_split
#'@importFrom stringr str_c
#'@importFrom utils read.csv
#'@import readr
#'@export


combineCSV <- function(fpath, dataname = NULL){ #see the r base cheatsheet for function syntax

  # List all files in the path
  f <- list.files(path = fpath, recursive = TRUE) #see ?list.files

  f <- f[!grepl(".(r|R)[a-z]+$", f)] #Get rid of any .R files in the folder, such as if you're running the script from the working directory
  #separate files list and place into tibble (This is all the names and subids in the titles of the files)
  f_split <- as_tibble(str_split(f, "_|/", simplify = TRUE)) %>% #see ?str_split from stringr package, separated by "_"
    mutate(.id = as.character(row_number())) %>% #How does making a row number column help this process?
    select(".id", everything()) #How does everything() work?

  #read in all files to list
  fls <- suppressWarnings(lapply(str_c(fpath,"/", f),
                                 function(x) read_delim(x,delim = ",",
                                                               col_names = F,
                                                               col_types = cols(.default = "c")))) #see ?str_c from stringr and ?lapply from the apply family



  #convert list to data frame
  d <- as.data.frame(bind_rows(fls, .id = ".id")) #see ?rbind_list from the data.table package


  #join data with identifying information
  e <- f_split %>% #Join df of names and ids with df of data
    full_join(d, by = ".id") #See ?full_join

  assign(deparse(substitute(dataname)), e, envir=.GlobalEnv)
}




