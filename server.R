library(ggplot2)
library(lubridate)
library(lazyeval)
source('config.R')

shinyServer(function(input, output, session) {
  
  #######################
  #   Table retrieval   #
  #######################
  
  getTableDef <- function(columns, tname){
    filter(columns, tablename == tname) %>%
      select(column, type)
  }
  
  
  getColumnType <- function(columns, tname, col){
    getTableDef(columns, tname) %>%
      filter(column == col) %>% select(type) %>% as.character()
    
  }
  
  getTableSample <- function(tname){
    readRDS(paste0('tables/', tname, '.rda'))
  }
  
  getTableFull <- function(table, db){
    tbl(db, table)
  }
  
  applyFilters = function(table){
    columnsFilter = input$filterColumns
    
    for(col in columnsFilter) {
      type = getColumnType(allColumns, tableName(), col)
      ftype = input[[paste0("filterType", col,sep="_")]]
      
      if(length(ftype)>0){
        if(ftype == 'If missing'){
          table = filter_(table, paste0("is.na(",col,")"))
        }else{
          if(type == 'character' | type == 'boolean'){
            values = input[[paste0("filter", col,sep="_")]]
            values = ifelse(values == '<<EMPTY>>', '', values)
            
            if(length(values) > 0){
              if(length(values) == 1){
                formula_string = paste0(col, " %in% (v)")
              }else{
                formula_string = paste0(col, " %in% v")
              }
              
              if (ftype == "If true"){
                table = filter_(table, interp(formula_string, v = values))
              }else{
                table = filter_(table, interp(paste0("!", formula_string), v = values))
              }
            }
          }else if (type == 'numeric' | type == 'integer'){
            values = input[[paste0("filter", col,sep="_")]]
            
            if (ftype == "If true"){
              table = filter_(table, interp(paste0(col, " >= x & ", col, " <= y"), .values = list(x=values[1], y=values[2])))
            }else{
              table = filter_(table, interp(paste0(col, " < x & ", col, " > y"), .values = list(x=values[1], y=values[2])))
            }
          }else if (type == 'date' | type == 'datetime'){
            values = input[[paste0("filter", col,sep="_")]]
            
            minDay=as.Date(values[1])
            maxDay=as.Date(values[2])+1
            
            if (ftype == "If true"){
              table = filter_(table, paste0(col, " >= '", minDay, "' & ", col, " < '", maxDay, "'"))
            }else{
              table = filter_(table, paste0(col, " < '", minDay, "' & ", col, " >= '", maxDay, "'"))
            }
          }
        }
      }
      
      
      table
    }
    
    table
    
  }
  
  # Load schema
  load('schema.rda')
  
  
  #######################
  #   Reactive tables   #
  #######################
  
  tableDef = reactive({
    if(length(input$selectedTable) > 0){
      if(!is.na(input$selectedTable)){
        t = getTableDef(allColumns, input$selectedTable)
        f = function(x) {getColumnType(allColumns, input$selectedTable, x)}
        t$resolved_type = lapply(t$column, f)
        t
      }
    }else{
      data.frame(column="A dataset", resolved_type='character')
    }
  })
  
  sampleTable = reactive({
    if(length(input$selectedTable) > 0){
      if(!is.na(input$selectedTable)){
          getTableSample(input$selectedTable)
      }
    }else{
      data.frame(choose="A dataset")
    }
  })
  
  tableName = reactive({
    input$selectedTable
  })

  output$tableDetail <- renderDataTable({
    sampleTable()
  })
  
  
  output$tableRender = renderUI({
    selectInput('selectedTable', label='Table', choices=allTables$tablename)
  })
  
  
  #######################
  #      Scatterplot    #
  #######################
  
  scatterResolve = reactive({
    validate(
      need(input$scatterX != '' & input$scatterY != '' && input$scatterX != input$scatterY, "Please select two different X and Y to create the chart")
    )
    
    if(input$scatterX != '' & input$scatterY != '' && input$scatterX != input$scatterY){
      
      withProgress(message = 'Generating chart...', value = 0, {
      txt = ""
      
      xType = getColumnType(allColumns, tableName(), input$scatterX)
      yType = getColumnType(allColumns, tableName(), input$scatterY)
      cType = getColumnType(allColumns, tableName(), input$scatterC)
      
      if(input$sampleMode){
        txt = paste0(txt, "--Remember you are in SAMPLE MODE (Random 5k sample from dataset)--")
        origData = getTableSample(input$selectedTable)
      }else{
        dbcon <- dplyr_DB()
        origData = getTableFull(input$selectedTable, dbcon)
      }
      incProgress(0.1)
      origData = applyFilters(origData)
      
      incProgress(0.1)
      
      if(input$scatterC != ''){
        data = mutate_(origData, .dots=setNames(c(input$scatterX, input$scatterY, input$scatterC), c("x", "y", "group"))) %>%
          select(x,y,group) %>%
          collect(n=Inf)
        
        if(cType != 'integer' & cType != 'numeric' & select(data, group) %>% distinct() %>% count() > 30){
          txt = paste(txt, "Won't use color with more than 30 categories", sep="\n")
          data = select(data, -group)
        }
        
      }else{
        data = select_(origData, .dots=setNames(c(input$scatterX, input$scatterY), c("x", "y"))) %>% collect(n=Inf)
      }
      
      if(!input$sampleMode){
        rm(origData)
      }
      incProgress(0.4)
      
      if(xType == 'character'){
        if(select(data, x) %>% distinct() %>% count() > 20){
          txt = paste(txt, "* This may not be the best chart for the data, X is a categorical variable with too many possible values", sep='\n')
        }
        data$x = ifelse(is.na(data$x), 'NULL', data$x)
      }else if (xType == 'date'){
        data$x = as.Date(data$x)
      }else if (xType == 'datetime'){
        data$x = as.POSIXct(data$x)
      }
      
      # Check if Y is string
      if(yType == "character"){
        data$y = ifelse(is.na(data$y), 'NULL', data$y)
        
        if(select(data, y) %>% distinct() %>% count() > 20){
          txt = paste(txt, "This may not be the best chart for the data, Y is a categorical variable with too many possible values", sep="\n")
        }
      #Check if Y is date
      }else if (yType == 'date'){
        txt = paste(txt, "It's recommended to use date variables in the X axis, instead of Y", sep="\n")
        data$y = as.Date(data$y)
      
      #Check if Y is datetime
      }else if (yType == 'datetime'){
        txt = paste(txt, "It's recommended to use datetime variables in the X axis, instead of Y", sep="\n")
        data$y = as.POSIXct(data$y)
      }
      
      # Calculate different combinations
      cnt = select(data, x, y) %>% distinct() %>% count()
      incProgress(0.1)
      
      #Start generating plot
      g = ggplot(data, aes(x=x, y=y)) +
        labs(x=input$scatterX, y=input$scatterY)
      
      #Which aes to use
      if("group" %in% colnames(data))
      {
        aesUse = aes(colour=group)
        g = g + labs(colour=input$scatterC)
      }else{
        aesUse=NULL
      }
      
      # Jitter when there are not many possible combinations
      if(cnt < 100){
        g = g + suppressWarnings(geom_jitter(aesUse, alpha=0.4))
      }else{
        g = g + suppressWarnings(geom_point(aesUse, alpha=0.4))
      }
      
      # Log Scale X
      if((xType == 'integer' | xType == 'numeric') & input$scatterXlog){
        g = g + scale_x_log10()
      }
      
      # Log Scale Y
      if((yType == 'integer' | yType == 'numeric') & input$scatterYlog){
        g = g + scale_y_log10()
      }
      
      if((xType == 'integer' | xType == 'numeric') & (yType == 'integer' | yType == 'numeric')){
        g = g +
          suppressWarnings(stat_ellipse(type = "norm", linetype = 2)) +
          suppressWarnings(stat_ellipse(type = "t"))
      }
      
      if (xType == 'date'){
        g = g + scale_x_date()
      }else if (xType == 'datetime'){
        g = g + scale_x_datetime()
      }
      
      if (yType == 'date'){
        g = g + scale_y_date()
      }else if (yType == 'datetime'){
        g = g + scale_y_datetime()
      }
      
      g = g + theme_bw() + theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1),
        legend.position = "top"
        )
      incProgress(0.3)
      
      list(chart=g, text=txt)
      
    })
    }else{
      list(chart=NULL, text=NULL)
    }
  })
  
  output$scatterMessages = renderText({
    s = scatterResolve()
    s$text
  })
  
  output$scatterPlot = renderPlot({
    s = scatterResolve()
    s$chart
  })
  
  output$downloadScatter <- downloadHandler(
   filename = function() {
     paste('chart-', Sys.time(), '.png', sep='')
   },
   content = function(file) {
     s = scatterResolve()
     ggsave(file = file, plot = s$chart, dpi = 128)
   }
  )
  
  #######################
  #     Line chart      #
  #######################
  
  output$lineYSum = renderUI({
    validate(
      need(input$lineY != '', '')
    )
    col = input$lineY
    type = getColumnType(allColumns, tableName(), col)
    
    choices = list('Count'='count')
    
    if(type %in% c('integer', 'numeric')){
      choices = list('Distribution'='dist', 'Count'='count', 'Sum values'='sum', 'Minimun value'='min', 'Maximun value'='max')
    }else if(type == 'boolean'){
      choices = list('Sum values'='sum', 'Count'='count')
    }
    
    selected = 'count'
    if(length(input$lineSum) > 0){
      if(input$lineSum %in% choices){
        selected = input$lineSum
      }
    }
    
    selectInput('lineSum', label='Aggregation', choices=choices, selected=selected)
  })
  
  output$lineXGranularity = renderUI({
    validate(
      need(input$lineX != '', '')
    )
    col = input$lineX
    type = getColumnType(allColumns, tableName(), col)
    
    choices = list()
    
    if(type == 'date'){
      choices = list('Yearly'='year', 'Monthly'='month', 'Weekly'='week', 'Daily'='day')
    }else if(type == 'datetime'){
      choices = list('Yearly'='year', 'Monthly'='month', 'Weekly'='week', 'Daily'='day', 'Hourly'='hour', 'Minutes'='minute', 'Seconds'='second')
    }else{
      return(HTML(""))
    }
    
    selected = 'month'
    if(length(input$lineDateGranularity) > 0){
      if(input$lineDateGranularity %in% choices){
        selected = input$lineDateGranularity
      }
    }
    selectInput('lineDateGranularity', label='Date granularity', selected='month',
                choices=choices)
  })
  
  
  lineResolve = reactive({
    validate(
      need(input$lineX != '' & input$lineY != '' & input$lineX != input$lineY & input$lineY != input$lineGroup & input$lineSum != '', "Please select two different X and Y to create the chart")
    )
  
    withProgress(message = 'Generating chart...', value = 0, {
      txt = ""
      
      xType = getColumnType(allColumns, tableName(), input$lineX)
      yType = getColumnType(allColumns, tableName(), input$lineY)
      
      if(input$sampleMode){
        txt = paste0(txt, "--Remember you are in SAMPLE MODE (Random 5k sample from dataset)--")
        origData = getTableSample(input$selectedTable)
      }else{
        dbcon <- dplyr_DB()
        origData = getTableFull(input$selectedTable, dbcon)
      }
      incProgress(0.1)
      origData = applyFilters(origData)
      
      incProgress(0.1)
      if(input$lineGroup != ''){
        data = select_(origData, .dots=setNames(c(input$lineX, input$lineY, input$lineGroup), c("x", "y", 'g')))
      }else{
        data = select_(origData, .dots=setNames(c(input$lineX, input$lineY), c("x", "y"))) %>%
          mutate('g'=input$lineY)
      }
      
      ## Transform date
      if(xType %in% c('date', 'datetime')){
        if ("tbl_postgres" %in% class(data)) {
          mutate_expression <-
            sprintf("x = DATE_TRUNC('%s', x)",
                    input$lineDateGranularity
            )
          
          mutate_command <-
            parse(text=sprintf("dplyr::mutate(data, %s)",
                               mutate_expression
            )
            )
          data = eval(mutate_command)
        }else{
          data = collect(data,n=Inf) # If not supported, bring data.frame
          data = mutate(data, x=floor_date(as.POSIXct(x), input$lineDateGranularity))
        }
      }
      
      incProgress(0.2)
        
      data=group_by(data, x, g) %>% filter(!is.na(y))
      
      if(input$lineSum=='count'){
        if(yType == 'character'){
          data = summarize(data, y=sum(ifelse(y != '', 1, 0)))
        }else{
          data = tally(data) %>% select(x, g, y=n)
        }
      }else if(input$lineSum=='sum'){
        if(yType == 'boolean'){
          
          data = summarize(data, y=sum(ifelse(y == TRUE, 1, 0)))
        }else{
          data = summarize(data, y=sum(y))
        }
        
      }else if(input$lineSum=='min'){
        data = summarize(data, y=min(y))
      }else if(input$lineSum=='max'){
        data = summarize(data, y=max(y))
      }else if(input$lineSum=='dist'){
        
        if ("tbl_postgres" %in% class(data)) {
          data = mutate(data,
                          rnk__ = ntile(y, 100)
                        ) %>%
            summarize(
              vl=max(ifelse(rnk__ <= 5, y, NA)),
              l=max(ifelse(rnk__ <= 25, y, NA)),
              h=min(ifelse(rnk__ >= 75, y, NA)),
              vh=min(ifelse(rnk__ >= 95, y, NA)),
              y=min(ifelse(rnk__ >= 50, y, NA))
            )
        }else{
          data = collect(data,n=Inf) # If not supported, bring data.frame
          data = summarize(data,
                           vl=quantile(y, probs=(0.05), na.rm=T),
                           l=quantile(y, probs=(0.25), na.rm=T),
                           h=quantile(y, probs=(0.75), na.rm=T),
                           vh=quantile(y, probs=(0.95), na.rm=T),
                           y=median(y, na.rm=T)
          )
        }
        
      }
      
      incProgress(0.3)
      
      data = collect(data, n=Inf) %>% arrange(g, x) %>% filter(!is.na(x))
      
      incProgress(0.25)
      
      if(xType %in% c('date', 'datetime')){
        data$x = as.POSIXct(data$x, format='%Y-%m-%d %H:%M:%S')
      }

      #Start generating plot
      g = ggplot(data, aes(x=x, y=y, fill=g, colour=g), colour='blue', fill='grey') +
        labs(x=input$lineX, y=input$lineY, fill=input$lineGroup, colour=input$lineGroup)
      
      g = g + geom_line()
      if(input$lineSum=='dist'){
        g = g +
          geom_ribbon(aes(ymin=l, ymax=h), alpha=0.2, colour=NA) +
          geom_ribbon(aes(ymin=vl, ymax=vh), alpha=0.2, colour=NA)
      }
      
      
      list(chart=g, text=txt)
      
    })
  })
  
  output$lineMessages = renderText({
    l = lineResolve()
    l$text
  })
  
  output$linePlot = renderPlot({
    l = lineResolve()
    l$chart
  })
  
  output$downloadLine <- downloadHandler(
    filename = function() {
      paste('chart-', Sys.time(), '.png', sep='')
    },
    content = function(file) {
      l = lineResolve()
      ggsave(file = file, plot = l$chart, dpi = 128)
    }
  )
  
  #######################
  #       Filters       #
  #######################
  
  
  output$filtersList = renderUI({
    selectizeInput(
      'filterColumns', 'Filter by', choices = tableDef()$column,
      multiple = TRUE
    )
  })
  
  output$filtersChoose = renderUI({
    columnsFilter = input$filterColumns
    sample = sampleTable()
    
    generateFilter = function(col){
      type = getColumnType(allColumns, tableName(), col)
      values = sample[,col]
      
      curSelected = isolate(input[[paste0("filter", col,sep="_")]])
      
      el = NULL
      if(type == 'character'){
        distinct = unique(values)
        distinct = ifelse(distinct == '', '<<EMPTY>>', distinct)
        
        el = selectizeInput(paste0("filter", col,sep="_"), col, selected=curSelected, choices=distinct, multiple=T, options = list(create = T, allowEmptyOption=T))
      }else if (type == 'boolean'){
        el = selectInput(paste0("filter", col,sep="_"), col, choices=c("TRUE", "FALSE", '<<EMPTY>>'), selected=curSelected)
      }else if (type == 'numeric' | type == 'integer'){
        
        minim = min(c(0,values), na.rm=T)
        least = quantile(values, probs = 0.25, na.rm = T)
        great = quantile(values, probs = 0.75, na.rm = T)
        step = 0.01
        if(all.equal(values, as.integer(values)) == TRUE){
          step = 1
        }
        
        if(length(curSelected)>0){
          valSel = curSelected
        }else{
          valSel = c(minim, great)
        }
        
        el = sliderInput(paste0("filter", col,sep="_"), col, value=valSel , min=minim, max=max(values,na.rm=T)+great/2, step=step)
      }else if (type == 'date' | type == 'datetime'){
        
        if(length(curSelected)>0){
          st = curSelected[1]
          en = curSelected[2]
        }else{
          st = as.Date(max(values, na.rm=T))-30
          en = max(values, na.rm=T)
        }
        
        el = dateRangeInput(paste0("filter", col,sep="_"),
                       label = col,
                       start = st, end = en
        )
      }
      
      fluidRow(
        column(
          width=9,
          el
        ),
        column(
          width=3,
          selectInput(paste0("filterType",col,sep="_"), '', choices=c('If true', 'If false', 'If missing'), selected="Include")
        )
      )
    }
    
    lapply(columnsFilter, generateFilter)
  })
  
  
  
  #######################
  # Render dynamic view #
  #######################
  
  output$outView = renderUI({
    switch(input$chart,
      'sample'={
        dataTableOutput('tableDetail')
      },
      'scatter'={
        span(
          fluidRow(
            column(width=10, offset=1,
            wellPanel(
              tabsetPanel(
                tabPanel("Basic",
                  fluidRow(
                    column(width=4,
                      selectInput('scatterX', label='X axis', choices=c('',tableDef()$column))
                    ),
                    column(width=4,
                      selectInput('scatterY', label='Y axis', choices=c('',tableDef()$column))
                    ),
                    column(width=4,
                      selectInput('scatterC', label='Color (Optional)', choices=c('',tableDef()$column))
                    )
                  )
                ),
                tabPanel("Advanced",
                   fluidRow(
                     column(width=3,
                            checkboxInput('scatterXlog', "Log scale on X")
                     ),
                     column(width=3,
                            checkboxInput('scatterYlog', "Log scale on Y")
                     )
                   )
                )
              )
            )
          )
        ),
        fluidRow(
          column(width=10, offset=1,
                 verbatimTextOutput('scatterMessages')     
          )
        ),
        fluidRow(
          column(width=10, offset=1,
            plotOutput('scatterPlot',
                       height=500)
          )
        ),
        fluidRow(
          column(width=10, offset=1,
             wellPanel(
               downloadButton('downloadScatter', 'Download this chart')
             )
          )
        )
        )
      },
      'line'={
        lineColumns = filter(tableDef(), resolved_type %in% c('date', 'datetime', 'integer', 'numeric')) %>% select(column)
        charColumns = filter(tableDef(), resolved_type == 'character') %>% select(column)
        span(
          fluidRow(
            column(width=10, offset=1,
                   wellPanel(
                     tabsetPanel(
                       tabPanel("Basic",
                                fluidRow(
                                  column(width=3,
                                         selectInput('lineX', label='X axis', choices=c('',lineColumns$column))
                                  ),
                                  column(width=3,
                                         uiOutput('lineXGranularity')
                                         
                                  ),
                                  column(width=3,
                                         selectInput('lineY', label='Y axis', choices=c('',tableDef()$column))
                                  ),
                                  column(width=3,
                                         uiOutput('lineYSum')
                                  )
                                ),
                                fluidRow(
                                  column(width=3,
                                         selectInput('lineGroup', label='Group by', choices=c('',charColumns$column))
                                  )
                                )
                       ),
                       tabPanel("Advanced",
                                fluidRow(
                                  column(width=3,
                                         checkboxInput('lineYlog', "Log scale on Y")
                                  )
                                )
                       )
                     )
                   )
            )
          ),
          fluidRow(
            column(width=10, offset=1,
                   verbatimTextOutput('lineMessages')     
            )
          ),
          fluidRow(
            column(width=10, offset=1,
                   plotOutput('linePlot',
                              height=500)
            )
          ),
          fluidRow(
            column(width=10, offset=1,
                   wellPanel(
                     downloadButton('downloadLine', 'Download this chart')
                   )
            )
          )
        )
      }
    )
  })
  
  output$tbl_name <- renderText({ tableName() })
  
  
})
