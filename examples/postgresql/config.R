library(DBI)
library(dplyr)
library(shiny)

#Dplyr database, replace this with a call to src_* with your database
dplyr_DB = function(){
  src_postgres()
}

#Function to execute arbitrary SQL
queryDb = function(query){
  db = dplyr_DB()
  dbGetQuery(db$obj, query)
}

#UI customizations
UI_HEADER=HTML("")
UI_THEME=NA
UI_STYLE=HTML("")
UI_TITLE="Chart builder"
