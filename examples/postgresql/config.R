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
UI_HEADER = commonHeader()
UI_THEME=baseTheme()
UI_STYLE=baseStyle()
UI_TITLE="Chart builder"
