#'Updates an existing data table
#'
#'@param tbl - the table you'd like to perform an update on 
#'@param set - a list (or vector/matrix/data.frame) containing vectors of values you'd like to update the selected rows with. Each different column change should be an element. Each list element is a different query.
#'@param where - a vector of where conditions for each of your updates
#'
#'@examples
#'sql_update("table_1", list(c("c1 = 301", "c2 =307"), c("c1 =909")), c("c1=300","c1 =99"))
#'@export
sql_update <- function(tbl, set, where){
  
  if(class(set)=="character"){
    set <- list(set)
  }
  #puts it into a list if it was in a dataframe
  else if(class(set)=="data.frame"){
    set <- as.list(as.data.frame(t(set)))
  }
  #puts it into a list if it was a matrix
  else if(class(set)=="matrix"){
    set <- lapply(1:nrow(set), function(i) set[i,])
  }
  if(length(set)!=length(where)){
    stop("Inconsistent number of queries requested (length of set and where not equal)")
  }
  #loops through values and inserts all the values
  for(i in 1:length(set)){
    set_str <- paste(set[[i]], collapse = ",")
    input <- sprintf("UPDATE %s SET %s WHERE %s;", tbl, set_str, where[i])
    dbSendQuery(connection, input)
  }
  message("Successful update!")
}