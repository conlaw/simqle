#'Inserts data into tables with SQL queries
#'
#'@param table - the name of the table to insert into
#'@param columns - a vector of the columns to insert into
#'@param values - a vector of the values to insert
#'
#'@examples
#'sql_insert("people", c("name", "sex", "age"), c("Maria", "female", "28"))
#'@export

sql_insert <- function(table, values, columns = -1){
  
  #checks if there's a connection and stops if it hasn't been initialized
  if(!exists("connection")){
    stop("There is no connection")
  }
  #creates the columns string
  if(columns!=-1) {
    columns_str <- paste(columns, collapse = ",")
    #splice together some brackets and spacing to help with query formation
    columns_str<- paste(" (", columns_str, ")", collapse="")
  }
  #if not specified assume that they want every column and the values column is not needed so make it nonexistant
  else columns_str <- ""
  
  #fixes data frames and matrices if a column is a string
  for(i in 1:ncol(values)){
    if(class(values[,i])=="factor"){
      values[,i] <- as.character(values[,i])
    }
    if(class(values[,i])=="character"){
      values[,i] <- paste("\"",values[,i], "\"", sep = "")
    }
  }
  
  #Makes values into a list if it isn't already (used if they input just a single character value not in a list so that below code still works)
  if(class(values)=="character"){
    values <- list(values)
  }
  #puts it into a list if it was in a dataframe
  else if(class(values)=="data.frame"){
    values <- as.list(as.data.frame(t(values)))
  }
  #puts it into a list if it was a matrix
  else if(class(values)=="matrix"){
    values <- lapply(1:nrow(values), function(i) values[i,])
  }
  #loops through values and inserts all the values
  for(i in 1:length(values)){
    values_str <- paste(values[[i]], collapse = ",")
    input <- sprintf("INSERT INTO %s%s VALUES (%s);", table, columns_str, values_str)
    dbSendQuery(connection, input)
  }
  message("Successful insert!")
}