# Joel’s awesome function -------------------------------------------------
#' Combine .csv Files
#'
#' Use this function to read in a series of .csv files and transform them into one data.frame. Make sure to save the output to a data.frame. See Examples for more.
#'
#' @param fpath The direcotry that contains all of the .csv files to be combined.
#' @param dataname The name you would like the combined .csv output data.frame to have.
#' @param headers Default value is FALSE. If you have column names, or Headers on each of your files, set to TRUE.
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


combineCSV <- function(fpath, dataname = NULL, headers = F){ #see the r base cheatsheet for function syntax

  # List all files in the path
  f <- list.files(path = fpath, recursive = TRUE) #see ?list.files

  f <- f[!grepl(".(r|R)[a-z]+$", f)] #Get rid of any .R files in the folder, such as if you're running the script from the working directory
  #separate files list and place into tibble (This is all the names and subids in the titles of the files)
  f_split <- as_tibble(str_split(f, "_|/", simplify = TRUE)) %>% #see ?str_split from stringr package, separated by "_"
    mutate(.id = as.character(row_number())) %>% #making a row number column allows the bind_rows argument later
    select(".id", everything()) #everything() grabs the rest of the columns and puts .id first

  #read in all files to list
  fls <- suppressWarnings(lapply(str_c(fpath,"/", f),
                                 function(x) read_delim(x,delim = ",",
                                                               col_names = headers,
                                                               col_types = cols(.default = "c")))) #see ?str_c from stringr and ?lapply from the apply family

  #convert list to data frame
  d <- as.data.frame(bind_rows(fls, .id = ".id")) #see ?rbind_list from the data.table package


  #join data with identifying information
  e <- f_split %>% #Join df of names and ids with df of data
    full_join(d, by = ".id") #See ?full_join

  assign(deparse(substitute(dataname)), e, envir=.GlobalEnv)
}


#' Name your dataframe properly
#'
#' Use this function to name your columns properly based on your experimental design
#'
#'
#' @param dataname The name you would like the combined .csv output data.frame to have.
#' @param subid The name or position of the column containing your subject IDs.
#' @param conditions The name, names, position, or positions of the column(s) containing your subject IDs.
#' @param dates_times The name or position of the column containing your dates or times.
#' @param drtIDs The name or position of the column containing your DRT IDs if you have multiple DRTs in one file.
#' @param trial The name or position of the column containing your trial count.
#' @param mics If you have microphones attached to your DRT, this will read the DRT output
#' @param hits The name or position of the column containing your hit count.
#' @param rt The name or position of the column containing your Response Times.
#' @examples
#'
#' #df <- nameCheck(combineCSV(fpath = getwd(), dataname = newdf), "V1",c("V3","V4"), "X1","X3","X4",mics = NULL,"X7","X19")
#'
#'@importFrom magrittr %>%
#'@import dplyr
#'@export


nameCheck <- function (dataname,subid,conditions,dates_times, drtIDs,trial, mics = NULL, hits, rt) {
  if (is.null(mics)) {
  dataname <- dataname %>%
      select(subid = subid, cond = conditions[1], dates_times = dates_times, drtIDs = drtIDs, hits = hits, rt = rt) %>%
    mutate(hits = as.numeric(hits), rt = as.numeric(rt))

  } else {
    dataname <- dataname %>%
      select(subid = subid, cond = conditions[1], dates_times = dates_times, drtIDs = drtIDs, mic = mics, hits = hits, rt = rt) %>%
      mutate(hits = as.numeric(hits), rt = as.numeric(rt))
  }
  return(dataname)
}

# nameCheck(df, "V1",c("V3","V4"), "X1","X3","X4",mics = NULL,"X7","X19") #Have to fix having 1 or more condition columns

