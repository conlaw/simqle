#'Deletes data from tables with SQL queries
#'
#'@param table the table to delete from
#'@param where_condition a string of conditions used to specify, need to use escape key for quotes
#'
#'@examples
#'sql_delete("people", c("name = \"Maria\" OR age = 28 AND gender = \"female\"")
#'sql_delete("car", c("make = \"Toyota\" AND model = \"Camry\""))
#'@export

sql_delete <- function(table, where_condition = "*"){
  
  if(!exists("connection")){
    stop("There is no connection")
  }
  if(where_condition == "*"){
    input <- sprintf("DELETE FROM %s", table)
  }
  else{
    input <- sprintf("DELETE FROM %s WHERE %s", table, where_condition)
  }
  
  dbSendQuery(connection, input)
  message("Successful deletion!")
}