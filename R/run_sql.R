## ------------------------------------------------------------------------- ##
#### -------------------------------------------- Read and run SQL scripts #### 

## Dependencies:  library(DBI)
##                library(glue)
##                library(stringr)
##                library(purrr)
##                library(readr)
##                Connection with DB must already exist

## This function reads SQL formatted files, divides it into multiple commands,
## and run them in sequence over the connected database. 
## All the commands in the SQL file will be called.
run_sql <- function(connection, query_path) {
  
  # Read SQL file 
  query <- readr::read_file(file = query_path)
  # Separate commands 
  query <- stringr::str_split(query, pattern = '--')
  # Transform to vector
  query <- purrr::as_vector(query)

  # Run queries in sequence
  for (i in seq_along(query)) {
    
    cat('\n')
    cat('## Query number:', i, '\n', sep = ' ')
    cat('--------------------', '\n')
    
    cat(glue::glue(query[i]), '\n')
    
    result <- DBI::dbExecute(
      conn = connection, 
      statement = glue::glue(query[i])
      )
    
    cat(result, '\n')
    cat('--------------------', '\n')
    
  }
  
}
## The format of the SQL file must follow one rule, each command must be 
## separated by empty comments --, with no other comments of this type anywhere
## in the file.
## ------------------------------------------------------------------------- ##