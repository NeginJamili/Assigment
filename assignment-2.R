library(tidyverse)


# Question 1 ------------------------------------------------------------------------------------------------------


<<<<<<< HEAD
#' Title
#' 
#' @param data 
#' @param column_prefix 
#'
#' @return A data frame with one column for the variables start with column_prefix
#' and another for their values
#' @export
#'
#' @examples
tidy_df <- function(data, column_prefix = "var"){
  data %>%
    gather(starts_with(column_prefix), key = "variable", value = "value") %>% 
    select(variable, value, everything())
}
# I reordered the table to have the columns that start with the given prefix as
# the first columns

# To check the function:
Example <- data.frame(
  var1 = c(1:3),
  var2 = c(6, 7, 4),
  others = c(4:6)
)
Example <- tidy_df(Example, "var")
Example
=======
tidy_df <- function(data, column_prefix = "var"){
  
}

>>>>>>> c08e94fea970d8d17be5a4ef61810367d94971f3

# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}

# extract_possible_names 




# Question 3 ------------------------------------------------------------------------------------------------------

# filter_names



# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
