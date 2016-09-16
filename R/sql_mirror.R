#' Takes a data frame and makes it into a table in a database
#' 
#' @param df - the dataframe to be placed in sql
#' 
#' @examples
#' sql_mirror(df)
#' 
#' @export

sql_mirror <- function(df){
  #ensures there's a db to connect to
  if(!exists("connection")){
    stop("There is no connection")
  }
  #ensures the object fed in is a dataframe
  if(class(df)!="data.frame"){
    stop("Please use a data frame object.")
  }
  message("Converting dataframe to table...")
  #gets the name of the df for the table
  tbl.name <- deparse(substitute(df))
  message(paste0("Table name: ",tbl.name))
  #Gets the column types and converts them to their SQL equivalent
  col.types <- sapply(df, class)
  col.sql.types <- sapply(1:length(df), function(x){
    message(paste("Converting column", names(col.types[x]),"..."))
    r_to_sql_types(col.types[x])
  })
  
  #creates the table
  sql_createtable(tbl.name, names(df), col.sql.types)
  
  #fills it with data from the table
  sql_insert(tbl.name, df)
  message("Table successfully created.")
}

r_to_sql_types <- function(type){
  if(type == "integer"){
    message("Implemented as type INTEGER")
    return("INTEGER")
  }
  else if(type == "numeric"){
    message("Implemented as type DOUBLE")
    return("DOUBLE")
  }
  else if(type == "logical"){
    message("Implemented as type BOOLEAN")
    return("BOOLEAN")
  }
  else{
    n<- max(sapply(names(tmp), nchar))
    message("Implemented as type VARCHAR("+n+")")
    varchar <- "VARCHAR"+n+")"
    return(varchar)
  }
}