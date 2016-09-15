#'Drops tables from a SQlite database
#'
#'@param table the table to delete from
#'
#'@examples
#'sql_droptable("Table_1")
#'@export

sql_droptable <- function(table_name) {
  if (!exists("connection")) {
    stop("There is no connection open.")
  }
  else {
    dbSendQuery(conn = connection, paste("DROP TABLE ", table_name))
    message("Drop successfull!")
  }
}