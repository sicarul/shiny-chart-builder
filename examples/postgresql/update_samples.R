source('config.R')
library(dplyr)

allTables = queryDb("
SELECT DISTINCT tablename
FROM pg_table_def
WHERE schemaname = 'public'
ORDER BY tablename
")


allColumns = queryDb("
SELECT \"tablename\", \"column\", \"type\"
FROM pg_table_def
WHERE schemaname = 'public'
")

allColumns$type = case_when(
  allColumns$type == 'integer' ~ 'integer',
  allColumns$type == 'bigint' ~ 'integer',
  startsWith(allColumns$type, 'numeric') ~ 'numeric',
  startsWith(allColumns$type, 'character') ~ 'character',
  allColumns$type == 'boolean' ~ 'boolean',
  allColumns$type == 'date' ~ 'date',
  startsWith(allColumns$type, "timestamp") ~ 'datetime',
  TRUE ~ 'character'
)

save(allTables, allColumns, file="schema.rda")

downloadSample = function(tab){
  print(paste0("Downloading sample for table ", tab))
  sample = queryDb(paste0('select * from ', tab, ' order by random() limit 5000'))
  saveRDS(sample, file=paste0('tables/', tab, '.rda'))
}

by(allTables, 1:nrow(allTables), function(row){
  tab = row$tablename

  if(!file.exists(paste0('tables/', tab, '.rda'))){
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
  }
}
)

#Delete old samples of non-existent tables
delete = data.frame(tablename=gsub('.rda', '', list.files('tables/', pattern="*.rda"))) %>% anti_join(allTables, by='tablename')

if(nrow(delete) > 0){
  res = by(delete, 1:nrow(delete), function(row){
    tab = row$tablename

    file.remove(paste0('tables/',tab,'.rda'))
  })
}
