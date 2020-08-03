
#' get_connected
#'
#' @description Build the connection between R session and PostgreSQL dbs
#'
#' @param projectName Character string for the project name. prefer to source
#'   from evnvironmental variables
#' @param credspath Character string to the credential file which has the
#'   authentification to access PostgreSQL dbs for projects.
#'
#' @return A PostgreSQL db connection.
#' @import DBI
#'         RPostgreSQL
#' @export
#'
#' @examples
#' get_connected()
get_connected <- function(projectName = Sys.getenv("projectName"), credspath = Sys.getenv("secret_path")){
  source(credspath)
  dbcreds = get(grep(projectName, ls(envir = .GlobalEnv), value = TRUE))
  connection = DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                              dbname = dbcreds$dbname,
                              host = dbcreds$host,
                              user = dbcreds$username,
                              password = dbcreds$password)
  return(connection)
}


#' get_info
#'
#' @description Basic description about the project db. Information such as how
#'   many tables, variables, number of pixels etc.
#'
#' @param projectName Character string for the project name. prefer to source
#'   from evnvironmental variables
#' @param dbconnection The connection to access the project db. The connection
#'   will be closed once the function is called.
#'
#' @seealso \code{\link{get_connected}}
#'
#' @import DBI
#' @return A print out description about the db and a data.frame which contains
#'   the factor table of the db.
#' @export
#'
#' @examples
#' get_info(projectName = Sys.getenv("projectName"), dbconnection = get_connected())
get_info <- function(projectName = Sys.getenv("projectName"), dbconnection = connection){

  # Table names
  Tables = dbListTables(connection)
  cat("The", projectName, "has", length(Tables), "tables :", Tables, "\r\n\r\n")
  # Pixels
  noofpixels = dbGetQuery(connection, statement = 'SELECT COUNT (*) FROM "Pixel"')
  cat("There are", noofpixels$count, "pixels in this project.\r\n\r\n")

  # Variables
  # tab_var = dbReadTable(connection, "Variable")
  # cat("There are", length(tab_var$variable), "variables in the Variable table:\r\n")
  # print(tab_var,"\r\n")


  ## Factors
  tab_Factor = dbReadTable(connection, "Factor")
  cat("There are", nrow(tab_Factor), "Factor in the Variable table. \r\n")
  print(tab_Factor)

  ## Clmiate combination
  # tab_Combination = dbReadTable(connection, "Combination")
  # cat("There are", nrow(tab_Combination), "Combination in the Variable table. \r\n")
  # tab_Combination

  # Close the db connection
  invisible(dbDisconnect(connection))
  noofpixels$count

  # Extra info what could display
  # tab_var = dbReadTable(connection, "Variable")
  # cat("There are", length(tab_var$variable), "variables in the Variable table. \r\n")
  # tab_var$variable
  #
  #
  # tab_RCP  = dbReadTable(connection, "RCP")
  # cat("There are", nrow(tab_RCP), "RCP(s) in the Variable table. \r\n")
  # tab_RCP
  #
  # tab_GCM = dbReadTable(connection, "GCM")
  # cat("There are", nrow(tab_GCM), "GCM(s) in the Variable table. \r\n")
  # tab_GCM
  #
  #
  # tab = dbReadTable(connection, "Pixel")
  # cat("There are", nrow(tab), "variables in the Variable table. \r\n")
  # tab
  return(tab_Factor)

}

#' get_table
#'
#' @description Retrieve the information from a table in the project DB.
#'
#' @param connection A PostgreSQL connection.
#' @param table Charater string to state which table is needed.
#'
#' @seealso \code{\link{get_connected}}
#' @import data.table
#' @return A data.table that contains the data from the desired table in DB.
#' @export
#'
#' @examples
#' get_table(get_connected(), table = "Variable")
get_table <- function(connection = get_connected(), table = "Variable"){
  tab = dbReadTable(connection, "Variable")
  tab = data.table::as.data.table(tab)
  return(tab)
}
#' get_data
#'
#' @description Retrieve selected/filtered data from the \strong{Simulation}
#'  table. The retrieved data will be saved in the project folder in a
#'  \strong{RDS} format.
#'
#' @param variables A named list. The element name must be the desired variable
#'   names and the values of the element are for filtering the DB. Default is to
#'   retrieve the entire \strong{Simulation} table. See details for constructing
#'    such named list.
#'
#' @param dbconnection A PostgreSQL connection.
#' @param path A character string to show where to save the retrieved data.
#'
#' @seealso \code{\link{get_table}}
#' @import data.table
#'         DBI
#' @return A data.table and a file saved in the path
#' @export
#'
#' @examples
#' get_data()
#'
get_data <- function(variables = NULL, dbconnection = connection, path = Sys.getenv("path")){
  ## Get all the whole Simulation table
  if(isTRUE(is.null(variables))){
    dt = data.table::as.data.table(DBI::dbReadTable(conn = connection, "Simulation"))
  } else if(isFALSE(is.list(variables))| is.null(names(variables))){
    print("variables must be a named list.")
  } else {
    sql = paste0('select * from "Simulation" where ')
    limitSql <- " \"%s\" IN ('%s') "
    no.ofvar = length(variables)
    querylist = vector("list", length = no.ofvar)
    for(i in seq_len(no.ofvar)){
      sqlm <- paste(sprintf(limitSql,
                            names(variables)[i],
                            paste(variables[[i]], collapse = "','")))
      querylist[[i]] <- sqlm

    }
    sql_updated = paste(sql, paste(querylist, collapse = "AND"))
    dt = data.table::as.data.table(dbGetQuery(get_connected(), sql_updated))

    dir_temp_data = paste0(path, "/Temp_Data")
    if(dir.exists(dir_temp_data)){
      saveRDS(dt, file = paste0(path,"/Temp_Data/temp.RDS"))
    } else{
      dir.create(dir_temp_data)
      saveRDS(dt, file = paste0(path,"/Temp_Data/temp.RDS"))
    }
    return(dt)
  }
}

#' get_graph
#'
#' @description Experimenting. Might not necessary. Plot box_plot for all
#'  variables in the \strong{Variable} table by the value group.
#'
#' @param plot_var A character vector that has the variables for y axis
#' @param group_var A character vector that has the variables for x axis -
#'   grouping variable
#' @param DT A data.table.
#'
#' @import ggplot2
#'         data.table
#'
#' @return \eqn{plot_var * group_var} ggplot objects
#' @export
#'
#' @examples
#' get_graph(DT, plot_var, group_var)
get_graph <- function(DT, plot_var, group_var){
  if(data.table::is.data.table(DT)){
    for (i in plot_var) {
      for (j in group_var){
        cols = c(i, j)
        p = DT[,..cols] %>%
          ggplot(aes_string(j, i, group = j)) +
          geom_boxplot() +
          geom_jitter(color = "red", alpha = 0.5)+
          theme_classic()
        cat("NOW plotting",cols, "\r\n")
        print(p)
      }
    }
  } else{
    print("DT must be data.table object.")
  }
  }
