#'Function to list tables for a connection
#'
#'@examples
#'sql_listtables()
#'
#'@export

sql_listtables <- function() {
  dbListTables(connection)
}