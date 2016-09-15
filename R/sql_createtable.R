#' Creates a new table within a database
#'
#' @param tbl_name - the name of the table to be made
#' @param col_names - a vector of the desired column names
#' @param col_types - a vector of the desired column types (length of col_names and col_types must be equal)
#' @param primary_key - a vector with the columns to be included in the primary key (default is all of them)
#'
#' @examples
#' sql_createtable("Table1", c("c1","c2"), rep("INTEGER",2), primary_key = "c1")
#' sql_createtable("Table2", c("c1","c2", "c3"), rep("INTEGER",3), primary_key = c("c1","c2"))
#' sql_createtable("Table3", c("c1","c2"), rep("INTEGER",2))
#' @export

sql_createtable <- function(tbl_name, col_names, col_types, primary_key = -1){
  if(!exists("connection")){
    stop("There is no connection open.")
  }
  #makes sure the right number of column names and types are included
  if(length(col_names) != length(col_types)){
    stop("There is a different number of columns given in the name and type fields. Creation cancelled.")
  }
  
  query <- paste("CREATE TABLE", tbl_name, "(", collapse = " ")
  #adds the column definitions
  for(i in 1:length(col_names)){
    column_def <- paste(col_names[i], toupper(col_types[i]), collapse = " ")
    column_def <- paste(column_def, ",", sep ="")
    query <- paste(query, column_def, sep = " ")
  }
  #adds the primary key definition
  if(primary_key == -1){
    primKey <- paste("PRIMARY KEY (", paste(col_names, collapse=", "), ")", sep = " ")
  }
  else{
    primKey <- paste("PRIMARY KEY (", paste(primary_key, collapse =", "), ")", sep = " ")
  }
  query <- paste(query, primKey, sep = " ")
  
  #finishes query
  query <- paste(query, ");", sep ="")
  #executes query
  dbSendQuery(connection, query)
  
  if(dbExistsTable(connection, tbl_name)){
    message("Successful table creation!")
  }
  else{
    message("Error with table creation!")
  }
}