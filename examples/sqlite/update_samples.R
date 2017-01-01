# Make sure your working directory is that of the project, not "example".
source('example/config.R')

allTables = queryDb("SELECT name tablename FROM sqlite_master WHERE type='table'")


getDef = function(table)
{
  t=queryDb(paste0("pragma table_info(", table, ")"))
  t$type = case_when(
    t$type == "INTEGER" ~ "integer",
    t$type == "REAL" ~ "numeric",
    t$type == "BOOLEAN" ~ "boolean",
    t$type == "TEXT" ~ "character",
    TRUE ~ "character"
  )
  t$tablename=table
  select(t, tablename, column=name, type)
}

allColumns = rowwise(allTables) %>% do(getDef(.))

#Save schema to disk
save(allTables, allColumns, file="schema.rda")

downloadSample = function(tab){
  print(paste0("Downloading sample for table ", tab))
  sample = queryDb(paste0('select * from ', tab, ' order by random() limit 5000'))
  saveRDS(sample, file=paste0('tables/', tab, '.rda'))
  TRUE
}

#Download 5k sample for all tables
res = by(allTables, 1:nrow(allTables), function(row){
  tab = row$tablename
  
  r <- NULL
  attempt <- 1
  while( is.null(r) && attempt <= 3 ) {
    attempt <- attempt + 1
    r = tryCatch({
      return(downloadSample(tab))
    }, error = function(e){
      print(paste("Error:  ",err, ". Retrying..."))
    })
  }
})

#Delete old samples of non-existent tables
delete = data.frame(tablename=gsub('.rda', '', list.files('tables/', pattern="*.rda"))) %>% anti_join(allTables, by='tablename')

res = by(delete, 1:nrow(delete), function(row){
  tab = row$tablename
  
  file.remove(paste0('tables/',tab,'.rda'))
})
