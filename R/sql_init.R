#'Creates a global connection to a database
#'
#'@param file - the name of the file to connect to
#'
#'@examples
#'sql_init()
#'sql_init("my_first_database.sqlite")
#'
#'@export

# This funciton checks if the SQLite database already exists on the file system.
sql_init <- function(file = -1) {
  fileExists = FALSE;
  if (file == -1) {
    connection <<- dbConnect(drv = SQLite())
    message("You've connected to a new temporary database, this will be deleted at the end of your session.")
  }
  else {
    fileExists = file.exists(file)
    connection <<- dbConnect(drv = SQLite(), file)
    
    if(fileExists){
      message("Successful connection to the specified database!")
    }
    else{
      message("Successful connection to a new database with your specified name!")
    }
  }
}


# testing the function