# Joel’s awesome function -------------------------------------------------
#Use this function to read in a series of csv files and transform them into one data.frame

combineCSV <- function(fPath){ #see the r base cheatsheet for function syntax
  # List all files in the path
  f <- list.files(path = fPath, recursive = TRUE) #see ?list.files

  #separate files list and place into tibble (This is all the names and subids in the titles of the files)
  f_split <- as_tibble(str_split(f, "_|/", simplify = TRUE)) %>% #see ?str_split from stringr package, separated by "_"
    mutate(.id = row_number()) %>% #How does making a row number column help this process?
    select(".id", everything()) #How does everything() work?

  #read in all files to list
  fls <- lapply(str_c(fPath, f), read.csv, header = FALSE) #see ?str_c from stringr and ?lapply from the apply family

  #convert list to data frame
  d <- as.data.frame(rbindlist(fls, fill = TRUE, idcol = TRUE)) #see ?rbind_list from the data.table package

  #join data with identifying information
  e <- f_split %>% #Join df of names and ids with df of data
    full_join(d, by = ".id") #See ?full_join
}
