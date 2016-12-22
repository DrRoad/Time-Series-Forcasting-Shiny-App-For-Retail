options(shiny.maxRequestSize=300*1024^2)
shinyServer(function(input, output){

  ######### Reading Sales CSV File ################
  
  fdata <- reactive({
    
      ########a###################
      infile1 <- input$file1
      if (is.null(infile1)) {
      return(NULL)
      }
      
      infile2 <- input$file2
      if (is.null(infile2)) {
        return(NULL)
      }
      
      infile3 <- input$file3
      if (is.null(infile3)) {
        return(NULL)
      }
      
      infile4 <- input$file4
      if (is.null(infile4)) {
        return(NULL)
      }
      ##################a###########
      
      
      d_sales<-fread(infile1$datapath,showProgress=F,header=T,sep=',',stringsAsFactors=FALSE,col.names=c("Date","product_id","loc_id","bnt","Sales"))
     
      # d_sales<-Sales
      # d_clnd_hier=clnd
      # d_prod_hier=prod
      # d_loc_hier=loc
      
      d_clnd_hier<-read.csv(infile2$datapath,header=T)
      d_prod_hier<-read.csv(infile3$datapath,header=T)
      d_loc_hier <-read.csv(infile4$datapath,header=T)
      
      
      colnames(d_sales)<-c("Date","product_id","loc_id","bnt","Sales")
      d_sales$bnt=NULL
      d_sales<-aggregate(Sales~Date+product_id+loc_id,data = d_sales,FUN = sum,na.rm=TRUE)
      

      d_merged<-merge(d_sales,d_clnd_hier,by.x="Date",by.y="Week_ID")
      d_merged<-merge(d_merged,d_prod_hier,by.x="product_id",by.y="CSKU_ID")
      d_merged<-merge(d_merged,d_loc_hier,by.x="loc_id",by.y="Store_ID")
      
      
  if(input$flevel=="Weekly")
    {
      if(input$flevel2=="Store")
      {
        demo<-d_merged[c(7,14,27,4)]
        demo$Week=substring(demo$Week,1,10)
        demo$Week <- as.Date(as.character(demo$Week),format="%d-%m-%Y")
        colnames(demo)=c("Date","product_name","Store_Name","Sales")  
        demo<-aggregate(Sales~Date+product_name+Store_Name,data = demo,FUN = sum,na.rm=TRUE)
          # print("check store")
      }
      else if(input$flevel2 =="Channel")
      {
        demo<-d_merged[c(7,14,31,4)]
        demo$Week=substring(demo$Week,1,10)
        demo$Week <- as.Date(as.character(demo$Week),format="%d-%m-%Y")
        colnames(demo)=c("Date","product_name","Channel_Name","Sales")  
        demo<-aggregate(Sales~Date+product_name+Channel_Name,data = demo,FUN = sum,na.rm=TRUE)
        
          # print("check channel")
      }       
    }
      
  else if(input$flevel=="Monthly")
  {
    
    if(input$flevel2=="Store")
    {
      demo<-d_merged[c(9,14,27,4)]
      demo$Month <- as.Date(as.yearmon(demo$Month,"%b - %y"))
      colnames(demo)=c("Date","product_name","Store_Name","Sales")  
      demo<-aggregate(Sales~Date+product_name+Store_Name,data = demo,FUN = sum,na.rm=TRUE)
    }
    else if(input$flevel2 =="Channel")
    {
      demo<-d_merged[c(9,14,31,4)]
      demo$Month <- as.Date(as.yearmon(demo$Month,"%b - %y"))
      colnames(demo)=c("Date","product_name","Channel_Name","Sales")  
      demo<-aggregate(Sales~Date+product_name+Channel_Name,data = demo,FUN = sum,na.rm=TRUE)
    } 
    
  }    
        
      demo<-arrange(demo,Date)
      return(demo)
 
      })
  
  ############## Select Product from Sales File ########## 
  output$select_product <- renderUI({ 
  
    selectInput('xcol', "Select Product", unique(sort(fdata()$product_name)))
  
  })
  
  ############## Select Store from Sales File ########## 
  output$select_store <- renderUI({ 
    if(input$flevel2=="Store")
      selectInput('ycol', "Select Store", unique(sort(fdata()$Store_Name)))
    else if(input$flevel2=="Channel")
      selectInput('ycol', "Select Channel", unique(sort(fdata()$Channel)))  
  })
  
  ########### Stats Function ##############
  mystats <- function(x, na.omit=TRUE) 
  {
    if (na.omit) x <- x[!is.na(x)]
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    min <- min(x)
    max <- max(x)
    med<-median(x)
    UC <- m+3*s
    LC <- m-3*s
    return(c(n=n,median=med, mean=m, stdev=s,min = min, max=max, UC=UC, LC=LC))
  }
  
  ############### Sales Data Table ################
  output$table <- renderDataTable({
    
    if(is.null(fdata()))
    {return ()}
    
    if(input$flevel=="Weekly")
    {
      if(input$flevel2=="Store")
      {
      data<-fdata()
      data <- data[data$product_name == input$xcol,]
      data <- data[data$Store_Name == input$ycol,]
      data
      }
      
      else if(input$flevel2=="Channel")
      {
        data<-fdata()
        data <- data[data$product_name == input$xcol,]
        data <- data[data$Channel_Name == input$ycol,]
        # data <- aggregate(Sales~Date+product_name+Channel_Name,data = data,FUN = sum,na.rm=TRUE)
        data
      }
      
    }
    
    else if(input$flevel=="Monthly")
    {
      if(input$flevel2=="Store")
      {
      dmsales<-MonthManp()
      data<-dmsales[[4]]
      data <- data[data$product_name == input$xcol,]
      data <- data[data$Store_Name == input$ycol,]
      data
      }
      
      else if(input$flevel2=="Channel")
      {
        dmsales<-MonthManp()
        data<-dmsales[[4]]
        data <- data[data$product_name == input$xcol,]
        data <- data[data$Channel_Name == input$ycol,]
        data
        # data <- aggregate(Sales~Date+product_id+loc_id+Channel_Name,data = data,FUN = sum,na.rm=TRUE)
      }
    }
  }, options = list(searching = FALSE),rownames=FALSE)
  
  ############## Best Forecast table ###########
  output$bfcast<-renderDataTable({
    new <- getbestforecast()
    new[[2]]
  },options = list(searching = FALSE,paging = FALSE),rownames=FALSE)
  
  ############# MAPE ##############
  output$mape<-renderValueBox({
    new <- getbestforecast()
    valueBox(tags$p(paste(new[[1]],"%"),style="font-family: Georgia;font-size: 80%;"),"Mean Absolute % Error",color = "red",icon=icon("exclamation-triangle"))
  })
  
  ############# Model ################
  output$mod<-renderValueBox({
    new <- getbestforecast()
    valueBox(tags$p(new[[3]],style="font-family: Georgia;font-size: 80%;"),"Model",color = "aqua")
  })
  
  ############ Best Graph ###########
  output$graph<-renderPlotly({
    new<-getbestforecast()
    new[[4]]
  })
  
  ############ Mape Graph ###########
  output$sgraph<-renderPlotly({
    new<-mapegraph()
    new
  })
  
  ########## Accuracy ############
  output$facc<-renderPrint({
    new <- getDataModel()
    new[[1]]
  })
  
  ############ Forecast Comparison ################
  output$fplot <- renderPlotly({
    new <- getDataModel()
    new[[3]]
  })
  
  ############## Forecast ##############
  output$fcast<-renderDataTable({
    new <- getDataModel()
    new[[2]]
  },options = list(searching = FALSE,paging = FALSE),rownames=FALSE)
  
  ############## Download Data ################
  output$dData <- downloadHandler(
    filename = function() { paste(input$xcol,'_',input$ycol, '.csv', sep='') },
    content = function(file) {
      write.csv(getDataModel()[[2]], file,row.names = F)
    }
  )
  
  

  
  
  ########## Data Manipulation Week ################
  WeekManp<-reactive({
    
    Sales<-fdata()
    #Sales<-demo
    if(input$flevel2=="Store")
    {
    #SubSales<-subset(Sales,product_name=="APPLE iPHONE 5s 16GB SILVER CONTRACT HANDSET" & Store_Name=="GB60936 Liverpool Experience", select = c("Date","product_name","Store_Name","Sales"))
    SubSales<-subset(Sales,product_name==input$xcol & Store_Name==input$ycol, select = c("Date","product_name","Store_Name","Sales"))
    }
    else if(input$flevel2=="Channel")
    {
      SubSales<-subset(Sales,product_name==input$xcol & Channel_Name==input$ycol, select = c("Date","product_name","Channel_Name","Sales"))
    }
    #SubSales$Date<-as.Date(SubSales$Date, format = "%d-%m-%y")
    len.data<-length(SubSales$Date)
    min.data<-SubSales$Date[1]
    max.data<-SubSales$Date[len.data]
    all.data<-as.data.frame(seq(min.data,max.data, by = "week"))
    colnames(all.data)<-c("Date")
    SubSales<-merge(all.data,SubSales,by.x ="Date",by.y = "Date",all = T)
    
    # SubSales=SubSales[-grep(53,week(SubSales$Date)),]
    # y=SubSales[-grep(53,week(SubSales$Date)),]
    # week(SubSales)
    #   str(SubSales) 
    
    ##### Storing as a List
    var_dm<-as.list(mystats(SubSales$Sales))
    
    ##### Imputing missing values
    SubSales$Sales[is.na(SubSales$Sales==TRUE)] <- var_dm$mean 
    
    SubSales$product_name[is.na(SubSales$product_name==TRUE)] <- input$xcol
    
    if(input$flevel2=="Store")
    {
        SubSales$Store_Name[is.na(SubSales$Store_Name==TRUE)] <- input$ycol
    }
        else if(input$flevel2=="Channel")
    {
       SubSales$Channel_Name[is.na(SubSales$Channel_Name==TRUE)] <- input$ycol
    }
    
    ##### Removing outliers
    SubSales$Sales[(SubSales$Sales>var_dm$UC)] <- var_dm$UC
   
    var_week <- week(SubSales$Date)[1]
    var_year <-year(SubSales$Date[1])
    var_split=0.9
    train_len=round(nrow(SubSales)*var_split)
    test_len=nrow(SubSales)
    var_len=round((test_len-train_len))
    var_maxDate <- max(SubSales$Date)
    
    
    #### Training Set
    training<-slice(SubSales,1:train_len) 
    #### Testing Set
    testing<-slice(SubSales,train_len+1:test_len)
    
    var_maxtdate<-max(training$Date)
    
    ts_training<-ts(training$Sales,freq=52,start=c(var_year,var_week))
    ts_Sales<-ts(SubSales$Sales,freq=52,start=c(var_year,var_week))
    
    tlist<-list(testing,ts_Sales,var_maxDate,SubSales,ts_training,var_maxtdate)
    return(tlist)
  })
  
  ############### Data Manipulation Month ##############
  MonthManp<-reactive({
    Sales<-fdata()
    #Sales=demo
    #Sales$Date<-as.Date(as.character(Sales$Date),format="%d-%m-%y")
    Sales$Date<-paste(month(Sales$Date,label=TRUE,abbr=TRUE),"-",year(Sales$Date))
    Sales$Date<-as.Date(as.yearmon(Sales$Date,"%b - %Y"))
    Sales<-arrange(Sales,Date)
    
    Sales$rno<-seq(1,length(Sales$Date))
    Sales$Date<-paste(month(Sales$Date,label=TRUE,abbr=TRUE),"-",year(Sales$Date))
    Sales1<-Sales 
    if(input$flevel2=="Store")
    {
      SubSales<-subset(Sales,product_name==input$xcol & Store_Name==input$ycol, select = c("Date","product_name","Store_Name","Sales"))
      #SubSales<-subset(Sales,product_name=="APPLE iPHONE 5s 16GB SILVER CONTRACT HANDSET" & Store_Name=="GB60936 Liverpool Experience", select = c("Date","product_name","Store_Name","Sales"))
    }
    else if(input$flevel2=="Channel")
    {
      SubSales<-subset(Sales,product_name==input$xcol & Channel_Name==input$ycol, select = c("Date","product_name","Channel_Name","Sales"))
    }
    
    SubSales$Date<-as.Date(as.yearmon(SubSales$Date,"%b - %Y"))
    len.data<-length(SubSales$Date)
    SubSales<-arrange(SubSales,Date)
    min.data<-SubSales$Date[1]
    max.data<-SubSales$Date[len.data]
    all.data<-as.data.frame(seq(min.data,max.data, by = "month"))
    colnames(all.data)<-c("Date")
    SubSales<-merge(all.data,SubSales,by.x ="Date",by.y = "Date",all = T)
    Sales2<-SubSales
    SubSales$Date<-paste(month(SubSales$Date,label=TRUE,abbr=TRUE),"-",year(SubSales$Date))
    
    ##### Storing as a List
    var_dm<-as.list(mystats(SubSales$Sales))
    var_dm2<-as.list(mystats(Sales2$Sales))
   
     ##### Imputing missing values
    SubSales$Sales[is.na(SubSales$Sales==TRUE)] <- var_dm$mean 
    Sales2$Sales[is.na(Sales2$Sales==TRUE)] <- var_dm2$mean
    
    ##### Removing outliers
    SubSales$Sales[(SubSales$Sales>var_dm$UC)] <- var_dm$UC
    Sales2$Sales[(Sales2$Sales>var_dm$UC)] <- var_dm2$UC
    
    var_split=0.8
    var_Month <- month(as.Date(as.yearmon(SubSales$Date,"%b - %Y")))[1]
    var_year <-year(as.Date(as.yearmon(SubSales$Date,"%b - %Y")))[1]
    
    
    train_len=round(nrow(SubSales)*var_split)
    test_len=nrow(SubSales)
    var_len=round((test_len-train_len))
    
    
    #### Training Set
    training<-slice(SubSales,1:train_len) 
    #### Testing Set
    testing<-slice(SubSales,train_len+1:test_len)
    
    var_maxDate <- max(as.Date(as.yearmon(SubSales$Date,"%b - %Y")))
    var_maxtdate<- max(as.Date(as.yearmon(training$Date,"%b - %Y")))
    
   
    ts_training<-ts(training$Sales,freq=12,start=c(var_year,var_Month))
    ts_Sales<-ts(SubSales$Sales,freq=12,start=c(var_year,var_Month))
    tlist<-list(testing,ts_Sales,var_maxDate,Sales1,ts_training,Sales2,var_maxtdate)
    return(tlist)
  })
  
  
  ############# Selecting Forecasting Algorithm #############
  getDataModel <- reactive({
    
    if(input$flevel=="Weekly")
    { 
      dmsales<-WeekManp()
      if(length(dmsales[[2]])>104)
      {
        if (input$var=="Arima")
        {
          fit <- auto.arima(dmsales[[2]])
        }
        else if (input$var=="Linear Model")
        {
          fit <- tslm(dmsales[[2]]~trend+season)
        }
        else if (input$var=="STLF")
        {
          fit <- stlf(dmsales[[2]])
        }
        else if (input$var=="Basic Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE)
        }
        else if (input$var=="Double Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[2]], gamma = FALSE)
        }
        else
        {
          fit <- HoltWinters(dmsales[[2]])
        }
      }
      else if(length(dmsales[[2]])<=104)
      {
        if (input$var=="Arima")
        {
          fit <- auto.arima(dmsales[[2]])
        }
        else if (input$var=="Linear Model")
        {
          fit <- tslm(dmsales[[2]]~trend+season)
        }
        else if (input$var=="Basic Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE)
        }
        else if (input$var=="Double Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[2]], gamma = FALSE)
        }
      
      }
    }
    else if(input$flevel=="Monthly")
    {
      dmsales<-MonthManp()
      if(length(dmsales[[2]])>24)
      {
        if (input$var=="Arima")
        {
          fit <- auto.arima(dmsales[[2]])
        }
        else if (input$var=="Linear Model")
        {
          fit <- tslm(dmsales[[2]]~trend+season)
        }
        else if (input$var=="STLF")
        {
          fit <- stlf(dmsales[[2]])
        }
        else if (input$var=="Basic Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE)
        }
        else if (input$var=="Double Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[2]], gamma = FALSE)
        }
        else
        {
          fit <- HoltWinters(dmsales[[2]])
        }
      }
      else if(length(dmsales[[2]])<=24)
      {
        if (input$var=="Arima")
        {
          fit <- auto.arima(dmsales[[2]])
        }
        else if (input$var=="Linear Model")
        {
          fit <- tslm(dmsales[[2]]~trend+season)
        }
        else if (input$var=="Basic Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE)
        }
        else if (input$var=="Double Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[2]], gamma = FALSE)
        }
        
      }
    }
    
    if(input$flevel=="Weekly")
      funcwfcast(dmsales[[1]]$Sales,dmsales[[2]],dmsales[[6]],input$ahead,fit)
    else if(input$flevel=="Monthly")
      funcmfcast(dmsales[[1]]$Sales,dmsales[[2]],dmsales[[7]],input$ahead,fit)
  })
  
  
  ############# Selecting Forecasting Algorithm #############
  getMapeModel <- reactive({
    
    if(input$flevel=="Weekly")
    { 
      dmsales<-WeekManp()
      if(length(dmsales[[2]])>104)
      {
        if (input$var=="Arima")
        {
          fit <- auto.arima(dmsales[[5]])
        }
        else if (input$var=="Linear Model")
        {
          fit <- tslm(dmsales[[5]]~trend+season)
        }
        else if (input$var=="STLF")
        {
          fit <- stlf(dmsales[[5]])
        }
        else if (input$var=="Basic Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[5]], beta = FALSE, gamma = FALSE)
        }
        else if (input$var=="Double Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[5]], gamma = FALSE)
        }
        else
        {
          fit <- HoltWinters(dmsales[[5]])
        }
      }
      else if(length(dmsales[[2]])<=104)
      {
        if (input$var=="Arima")
        {
          fit <- auto.arima(dmsales[[5]])
        }
        else if (input$var=="Linear Model")
        {
          fit <- tslm(dmsales[[5]]~trend+season)
        }
        else if (input$var=="Basic Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[5]], beta = FALSE, gamma = FALSE)
        }
        else if (input$var=="Double Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[5]], gamma = FALSE)
        }
       
      }
    }
    else if(input$flevel=="Monthly")
    {
      dmsales<-MonthManp()
      if(length(dmsales[[2]])>24)
      {
        if (input$var=="Arima")
        {
          fit <- auto.arima(dmsales[[5]])
        }
        else if (input$var=="Linear Model")
        {
          fit <- tslm(dmsales[[5]]~trend+season)
        }
        else if (input$var=="STLF")
        {
          fit <- stlf(dmsales[[5]])
        }
        else if (input$var=="Basic Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[5]], beta = FALSE, gamma = FALSE)
        }
        else if (input$var=="Double Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[5]], gamma = FALSE)
        }
        else
        {
          fit <- HoltWinters(dmsales[[5]])
        }
      }
      else if(length(dmsales[[2]])<=24)
      {
        if (input$var=="Arima")
        {
          fit <- auto.arima(dmsales[[5]])
        }
        else if (input$var=="Linear Model")
        {
          fit <- tslm(dmsales[[5]]~trend+season)
        }
        else if (input$var=="Basic Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[5]], beta = FALSE, gamma = FALSE)
        }
        else if (input$var=="Double Exponential Smoothing")
        {
          fit <- HoltWinters(dmsales[[5]], gamma = FALSE)
        }
        
      }
    }
    
    if(input$flevel=="Weekly")
      funcmwfcast(dmsales[[1]],dmsales[[5]],dmsales[[6]],fit)
    else if(input$flevel=="Monthly")
      funcmmfcast(dmsales[[1]],dmsales[[5]],dmsales[[7]],fit)
  })
  
  ################### MAPE GRAPH ##############
  mapegraph<-reactive({
    
    new<-getMapeModel()
    
    if(input$flevel=="Weekly")
    {
      dmsales<-WeekManp()
      
      len.data<-length(dmsales[[1]]$Date)
      min.date<-dmsales[[1]]$Date[1]
      max.date<-max(dmsales[[1]]$Date)
      var_maxDate <- dmsales[[6]]
      
      wdate<-seq.Date(min.date,max.date,by="week")
      #wdate=wdate[-grep(53,week(wdate))]
      fordate<-data.frame(seq(var_maxDate+7, by = "week", length.out = length(dmsales[[1]]$Sales)))
      colnames(fordate)<-c("Week")

        fplot<-plot_ly() %>% 
        add_lines(x = wdate, y =dmsales[[1]]$Sales ,color = I("black"), name = "Sales") %>%
        add_lines(x = new$Date, y = new$Forecast, color = I("blue"), name = "Forecast",connectgaps = TRUE)%>%
        layout(legend = list(orientation = 'h'))
    }      
    else if(input$flevel=="Monthly")
    {
      dmsales<-MonthManp()
      
      dmsales[[1]]$Date<-as.Date(as.yearmon(dmsales[[1]]$Date,"%b - %Y"))
      
      len.data<-length(dmsales[[1]]$Date)
      min.date<-dmsales[[1]]$Date[1]
      max.date<-max(dmsales[[1]]$Date)
      var_maxDate <- dmsales[[7]]
      
      mdate<-seq.Date(min.date,max.date,by="month")
      fordate<-data.frame(seq(var_maxDate+32, by = "month", length.out = length(dmsales[[1]]$Sales)))
      colnames(fordate)<-c("Month")
      
      fplot<-plot_ly() %>% 
        add_lines(x = mdate, y = dmsales[[1]]$Sales,color = I("black"), name = "Sales") %>%
        add_lines(x = new$Date, y = new$Forecast, color = I("blue"), name = "Forecast",connectgaps = TRUE)%>%
        layout(legend = list(orientation = 'h'))
    }
    
    return(fplot)
    
  })
  
  ############# Comparing Forecasts Graphs #############
  Forecastcomp<-reactive({
  if(input$flevel=="Weekly")
    { 
        dmsales<-WeekManp()
        if(length(dmsales[[2]])>104)
        {
           arima<- forecast(auto.arima(dmsales[[2]]),h=input$ahead)
           TS   <- forecast(tslm(dmsales[[2]]~trend+season),h=input$ahead)
           HW1  <- forecast(HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE),h=input$ahead)
           HW2  <- forecast(HoltWinters(dmsales[[2]], gamma = FALSE),h=input$ahead)
           HW3  <- forecast(HoltWinters(dmsales[[2]]),h=input$ahead)
           STLF <- stlf(dmsales[[2]],h=input$ahead,lambda=0)
           
           len.data<-length(dmsales[[4]]$Date)
           min.date<-dmsales[[4]]$Date[1]
           max.date<-dmsales[[4]]$Date[len.data]
           var_maxDate <- max(dmsales[[4]]$Date)
           
           wdate<-seq.Date(min.date,max.date,by="week")
           #wdate=wdate[-grep(53,week(wdate))]
           fordate<-data.frame(seq(var_maxDate+7, by = "week", length.out = input$ahead))
           colnames(fordate)<-c("Week")
           fplot<-plot_ly() %>% add_lines(x = wdate, y = dmsales[[2]],color = I("black"), name = "observed") %>%
             add_lines(x = fordate$Week, y = arima$mean, color = I("blue"), name = "Arima",connectgaps = TRUE)%>%
             add_lines(x = fordate$Week, y = TS$mean, color = I("red"), name = "Linear Model",connectgaps = TRUE)%>%
             add_lines(x = fordate$Week, y = HW1$mean, color = I("green"), name = "Basic Exponential Smoothing",connectgaps = TRUE)%>%
             add_lines(x = fordate$Week, y = HW2$mean, color = I("orange"), name = "Double Exponential Smoothing",connectgaps = TRUE)%>%
             add_lines(x = fordate$Week, y = HW3$mean, color = I("pink"), name = "Tripple Exponential Smoothing",connectgaps = TRUE)%>%
             add_lines(x = fordate$Week, y = STLF$mean, color = I("purple"), name = "STLF",connectgaps = TRUE)%>%
             layout(legend = list(orientation = 'h'))
         }
        else if(length(dmsales[[2]])<=104)
        {
        arima<- forecast(auto.arima(dmsales[[2]]),h=input$ahead)
        TS   <- forecast(tslm(dmsales[[2]]~trend+season),h=input$ahead)
        HW1  <- forecast(HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE),h=input$ahead)
        HW2  <- forecast(HoltWinters(dmsales[[2]], gamma = FALSE),h=input$ahead)
        
        len.data<-length(dmsales[[4]]$Date)
        min.date<-dmsales[[4]]$Date[1]
        max.date<-dmsales[[4]]$Date[len.data]
        var_maxDate <- max(dmsales[[4]]$Date)
        
        wdate<-seq.Date(min.date,max.date,by="week")
        #wdate=wdate[-grep(53,week(wdate))]
        fordate<-data.frame(seq(var_maxDate+7, by = "week", length.out = input$ahead))
        colnames(fordate)<-c("Week")
        fplot<-plot_ly() %>% add_lines(x = wdate, y = dmsales[[2]],color = I("black"), name = "observed") %>%
          add_lines(x = fordate$Week, y = arima$mean, color = I("blue"), name = "Arima",connectgaps = TRUE)%>%
          add_lines(x = fordate$Week, y = TS$mean, color = I("red"), name = "Linear Model",connectgaps = TRUE)%>%
          add_lines(x = fordate$Week, y = HW1$mean, color = I("green"), name = "Basic Exponential Smoothing",connectgaps = TRUE)%>%
          add_lines(x = fordate$Week, y = HW2$mean, color = I("orange"), name = "Double Exponential Smoothing",connectgaps = TRUE)%>%
          layout(legend = list(orientation = 'h'))
        }
    }
    
   else if(input$flevel=="Monthly")
    {
        dmsales<-MonthManp()
        if(length(dmsales[[2]])>24)
        {
      arima<- forecast(auto.arima(dmsales[[2]]),h=input$ahead)
      TS   <- forecast(tslm(dmsales[[2]]~trend+season),h=input$ahead)
      HW1  <- forecast(HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE),h=input$ahead)
      HW2  <- forecast(HoltWinters(dmsales[[2]], gamma = FALSE),h=input$ahead)
      HW3  <- forecast(HoltWinters(dmsales[[2]]),h=input$ahead)
      STLF <- stlf(dmsales[[2]],h=input$ahead,lambda=0)
    
      len.data<-length(dmsales[[6]]$Date)
      min.date<-dmsales[[6]]$Date[1]
      max.date<-max(dmsales[[6]]$Date)
      var_maxDate <- max(dmsales[[6]]$Date)
      
      mdate<-seq.Date(min.date,max.date,by="month")
      fordate<-data.frame(seq(var_maxDate+32, by = "month", length.out = input$ahead))
      colnames(fordate)<-c("Month")
      
      
      fplot<-plot_ly() %>% add_lines(x = mdate, y = dmsales[[2]],color = I("black"), name = "observed") %>%
      add_lines(x = fordate$Month, y = arima$mean, color = I("blue"), name = "Arima",connectgaps = TRUE)%>%
      add_lines(x = fordate$Month, y = TS$mean, color = I("red"), name = "Linear Model",connectgaps = TRUE)%>%
      add_lines(x = fordate$Month, y = HW1$mean, color = I("green"), name = "Basic Exponential Smoothing",connectgaps = TRUE)%>%
      add_lines(x = fordate$Month, y = HW2$mean, color = I("orange"), name = "Double Exponential Smoothing",connectgaps = TRUE)%>%
      add_lines(x = fordate$Month, y = HW3$mean, color = I("pink"), name = "Tripple Exponential Smoothing",connectgaps = TRUE)%>%
      add_lines(x = fordate$Month, y = STLF$mean, color = I("purple"), name = "STLF",connectgaps = TRUE)%>%
      layout(legend = list(orientation = 'h'))
      }
        else if(length(dmsales[[2]])<=24)
        {
        {
          arima<- forecast(auto.arima(dmsales[[2]]),h=input$ahead)
          TS   <- forecast(tslm(dmsales[[2]]~trend+season),h=input$ahead)
          HW1  <- forecast(HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE),h=input$ahead)
          HW2  <- forecast(HoltWinters(dmsales[[2]], gamma = FALSE),h=input$ahead)

          len.data<-length(dmsales[[6]]$Date)
          min.date<-dmsales[[6]]$Date[1]
          max.date<-max(dmsales[[6]]$Date)
          var_maxDate <- max(dmsales[[6]]$Date)
          
          mdate<-seq.Date(min.date,max.date,by="month")
          fordate<-data.frame(seq(var_maxDate+32, by = "month", length.out = input$ahead))
          colnames(fordate)<-c("Month")
          
          
          fplot<-plot_ly() %>% add_lines(x = mdate, y = dmsales[[2]],color = I("black"), name = "observed") %>%
            add_lines(x = fordate$Month, y = arima$mean, color = I("blue"), name = "Arima",connectgaps = TRUE)%>%
            add_lines(x = fordate$Month, y = TS$mean, color = I("red"), name = "Linear Model",connectgaps = TRUE)%>%
            add_lines(x = fordate$Month, y = HW1$mean, color = I("green"), name = "Basic Exponential Smoothing",connectgaps = TRUE)%>%
            add_lines(x = fordate$Month, y = HW2$mean, color = I("orange"), name = "Double Exponential Smoothing",connectgaps = TRUE)%>%
            layout(legend = list(orientation = 'h'))
        } 
      }
    }
    return(fplot)
  })
  

  ########## Weekly Forecast ############
  funcwfcast<-function(a,b,c,d,fit)
  {
    if(input$var=="STLF")
      fcast<-stlf(b,h=d,lambda=0)
    else
      fcast<-forecast(fit,h=d)
   
    dfcast<-data.frame(fcast)
    x<-data.frame(seq(as.Date(c+7), by = "week", length.out = d))
    ffcast<-as.data.frame(cbind(x,round(dfcast$Point.Forecast,digits = 2),round(dfcast$Lo.95,digits = 2),round(dfcast$Hi.95,digits = 2)))
    names(ffcast)<-c("Date","Forecast","Lo-95","Hi-95")
    facc<-accuracy(fcast$mean,a)
    
     fplot<-Forecastcomp()
    # fplot<-plot_ly() %>%
    #   add_lines(x = time(b), y = b,
    #             color = I("black"), name = "observed") %>%
    #   add_ribbons(x = time(fcast$mean), ymin = fcast$lower[,2], ymax = fcast$upper[,2],
    #               color = I("gray80"), name = "95% confidence") %>%
    #   add_lines(x = time(fcast$mean), y = fcast$mean, color = I("blue"), name = "prediction")
    
    flist<-list(facc,ffcast,fplot)
    return(flist)
  } 
  
  ########## Weekly MAPE Forecast ############
  funcmwfcast<-function(a,b,c,fit)
  {
    d=length(a$Sales)
    if(input$var=="STLF")
      fcast<-stlf(b,h=d,lambda=0)
    else
      fcast<-forecast(fit,h=d)
    
    dfcast<-data.frame(fcast)
    x<-data.frame(seq(as.Date(c+7), by = "week", length.out = d))
    ffcast<-as.data.frame(cbind(x,round(dfcast$Point.Forecast,digits = 2),round(dfcast$Lo.95,digits = 2),round(dfcast$Hi.95,digits = 2)))
    names(ffcast)<-c("Date","Forecast","Lo-95","Hi-95")
    return(ffcast)
  } 

  ########### Monthly Forecast #############
  funcmfcast<-function(a,b,c,d,fit)
  {
    if(input$var=="STLF")
      fcast<-stlf(b,h=d,lambda=0)
    else
      fcast<-forecast(fit,h=d)
    
    dfcast<-data.frame(fcast)
    x<-data.frame(seq(as.Date(c+32), by = "month", length.out = d))
    colnames(x)<-"Months"
    x<-paste(month(x$Months,label=TRUE,abbr=TRUE),"-",year(x$Months))
    
    ffcast<-as.data.frame(cbind(x,round(dfcast$Point.Forecast,digits = 2),round(dfcast$Lo.95,digits = 2),round(dfcast$Hi.95,digits = 2)))
    names(ffcast)<-c("Date","Forecast","Lo-95","Hi-95")
    facc<-accuracy(fcast$mean,a)
    fplot<-Forecastcomp()
    # fplot<-plot_ly() %>%
    #   add_lines(x = time(b), y = b,
    #             color = I("black"), name = "observed") %>%
    #   add_ribbons(x = time(fcast$mean), ymin = fcast$lower[,2], ymax = fcast$upper[,2],
    #               color = I("gray80"), name = "95% confidence") %>%
    #   add_lines(x = time(fcast$mean), y = fcast$mean, color = I("blue"), name = "prediction")
    flist<-list(facc,ffcast,fplot)
    return(flist)
  } 
  
  ########## Monthly MAPE Forecast ############
  funcmmfcast<-function(a,b,c,fit)
  {
    d=length(a$Sales)
    if(input$var=="STLF")
      fcast<-stlf(b,h=d,lambda=0)
    else
      fcast<-forecast(fit,h=d)
    
    dfcast<-data.frame(fcast)
    x<-data.frame(seq(as.Date(c+32), by = "month", length.out = d))
    ffcast<-as.data.frame(cbind(x,round(dfcast$Point.Forecast,digits = 2),round(dfcast$Lo.95,digits = 2),round(dfcast$Hi.95,digits = 2)))
    names(ffcast)<-c("Date","Forecast","Lo-95","Hi-95")
    return(ffcast)
  } 
  
  
  ########### Generating Best Forecast ##############
  getbestforecast<-reactive({
    
    if(input$flevel=="Weekly")
      {
        dmsales<-WeekManp()
        if(length(dmsales[[2]])>104)
        {
        fit1 <- auto.arima(dmsales[[2]])
        fit1fcast<-forecast(fit1,h=input$ahead)
        ArAcc<-as.data.frame(accuracy(fit1fcast$mean,dmsales[[1]]$Sales))
        
        fit2 <- tslm(dmsales[[2]]~trend+season)
        fit2fcast<-forecast(fit2,h=input$ahead)
        LmAcc<-as.data.frame(accuracy(fit2fcast$mean,dmsales[[1]]$Sales))
        
        fit3 <- stlf(dmsales[[2]])
        fit3fcast<-stlf(dmsales[[2]],h=input$ahead,lambda=0)
        STLFacc<-as.data.frame(accuracy(fit3fcast$mean,dmsales[[1]]$Sales))
        
        fit4 <- HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE)
        fit4fcast<-forecast(fit4,h=input$ahead)
        Hw1Acc<-as.data.frame(accuracy(fit4fcast$mean,dmsales[[1]]$Sales))
        
        fit5 <- HoltWinters(dmsales[[2]], gamma = FALSE)
        fit5fcast<-forecast(fit5,h=input$ahead)
        HW2Acc<-as.data.frame(accuracy(fit5fcast$mean,dmsales[[1]]$Sales))
        
        fit6 <- HoltWinters(dmsales[[2]])
        fit6fcast<-forecast(fit6,h=input$ahead)
        HW3Acc<-as.data.frame(accuracy(fit6fcast$mean,dmsales[[1]]$Sales))
        
        MAPE<-c(ArAcc$MAPE,LmAcc$MAPE,Hw1Acc$MAPE,HW2Acc$MAPE,HW3Acc$MAPE,STLFacc$MAPE)    #// All Models' Mape
        MAPE<-round(MAPE,digits=2)
        ModelForecast<-c("Arima","Linear Model","Basic Exponential Smoothing","Double Exponential Smoothing","Triple Exponential Smoothing","STLF")                               #// Model Names
        MinMape<-sort(MAPE)
        min_mape<-MinMape[1]
        Minmatch<-match(min_mape,MAPE) 
        var1 <- ModelForecast[Minmatch]
        
        var2<-switch(var1,
                     "Arima" = forecast(auto.arima(dmsales[[2]]),h=input$ahead),
                     "Linear Model"  = forecast(tslm(dmsales[[2]]~trend+season),h=input$ahead),
                     "Basic Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]],beta = FALSE,gamma = FALSE),h=input$ahead),
                     "Double Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]],gamma = FALSE),h=input$ahead),
                     "Triple Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]]),h=input$ahead),
                     "STLF" =stlf(dmsales[[2]],h=input$ahead,lambda=0)) 
        
        var2<-as.data.frame(var2)
      }
        else if(length(dmsales[[2]])<=104)
        {
        fit1 <- auto.arima(dmsales[[2]])
        fit1fcast<-forecast(fit1,h=input$ahead)
        ArAcc<-as.data.frame(accuracy(fit1fcast$mean,dmsales[[1]]$Sales))
        
        fit2 <- tslm(dmsales[[2]]~trend+season)
        fit2fcast<-forecast(fit2,h=input$ahead)
        LmAcc<-as.data.frame(accuracy(fit2fcast$mean,dmsales[[1]]$Sales))
        
        fit4 <- HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE)
        fit4fcast<-forecast(fit4,h=input$ahead)
        Hw1Acc<-as.data.frame(accuracy(fit4fcast$mean,dmsales[[1]]$Sales))
        
        fit5 <- HoltWinters(dmsales[[2]], gamma = FALSE)
        fit5fcast<-forecast(fit5,h=input$ahead)
        HW2Acc<-as.data.frame(accuracy(fit5fcast$mean,dmsales[[1]]$Sales))
        
        MAPE<-c(ArAcc$MAPE,LmAcc$MAPE,Hw1Acc$MAPE,HW2Acc$MAPE)    #// All Models' Mape
        MAPE<-round(MAPE,digits=2)
        ModelForecast<-c("Arima","Linear Model","Basic Exponential Smoothing","Double Exponential Smoothing")                               #// Model Names
        MinMape<-sort(MAPE)
        min_mape<-MinMape[1]
        Minmatch<-match(min_mape,MAPE) 
        var1 <- ModelForecast[Minmatch]
        
        var2<-switch(var1,
                     "Arima" = forecast(auto.arima(dmsales[[2]]),h=input$ahead),
                     "Linear Model"  = forecast(tslm(dmsales[[2]]~trend+season),h=input$ahead),
                     "Basic Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]],beta = FALSE,gamma = FALSE),h=input$ahead),
                     "Double Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]],gamma = FALSE),h=input$ahead)) 
        
        var2<-as.data.frame(var2)
      }
     
      y<-data.frame(seq(as.Date(dmsales[[3]]+7), by = "week", length.out = input$ahead))
      bfcast<-as.data.frame(cbind(y,round(var2$`Point Forecast`,digits = 2),round(var2$`Lo 95`,digits = 2),round(var2$`Hi 95`,digits = 2)))
      names(bfcast)<-c("Week","Forecast","Lo-95","Hi-95")
      
      len.data<-length(dmsales[[4]]$Date)
      min.date<-dmsales[[4]]$Date[1]
      max.date<-dmsales[[4]]$Date[len.data]
      var_maxDate <- max(dmsales[[4]]$Date)
      
      wdate<-seq.Date(min.date,max.date,by="week")
      #wdate=wdate[-grep(53,week(wdate))]
      fordate<-data.frame(seq(var_maxDate+7, by = "week", length.out = input$ahead))
      colnames(fordate)<-c("Week")
      
        fplot<-plot_ly() %>%
        add_lines(x = wdate, y = dmsales[[2]],
                  color = I("black"), name = "observed") %>%
        add_lines(x = fordate$Week, y = var2$`Point Forecast`, color = I("blue"),name="Forecast",connectgaps = TRUE)%>%
        add_ribbons(x = fordate$Week, ymin = var2$`Lo 95`, ymax = var2$`Hi 95`,color = I("gray80"), name = "95% confidence")%>%
        layout(legend = list(orientation = 'h'))
      
    fplot
   }  
    
    else if(input$flevel=="Monthly")
    {
      dmsales<-MonthManp()
      if(length(dmsales[[2]])>24)
      {
        fit1 <- auto.arima(dmsales[[2]])
        fit1fcast<-forecast(fit1,h=input$ahead)
        ArAcc<-as.data.frame(accuracy(fit1fcast$mean,dmsales[[1]]$Sales))
        
        fit2 <- tslm(dmsales[[2]]~trend+season)
        fit2fcast<-forecast(fit2,h=input$ahead)
        LmAcc<-as.data.frame(accuracy(fit2fcast$mean,dmsales[[1]]$Sales))
        
        fit3 <- stlf(dmsales[[2]])
        fit3fcast<-stlf(dmsales[[2]],h=input$ahead,lambda=0)
        STLFacc<-as.data.frame(accuracy(fit3fcast$mean,dmsales[[1]]$Sales))
        
        fit4 <- HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE)
        fit4fcast<-forecast(fit4,h=input$ahead)
        Hw1Acc<-as.data.frame(accuracy(fit4fcast$mean,dmsales[[1]]$Sales))
        
        fit5 <- HoltWinters(dmsales[[2]], gamma = FALSE)
        fit5fcast<-forecast(fit5,h=input$ahead)
        HW2Acc<-as.data.frame(accuracy(fit5fcast$mean,dmsales[[1]]$Sales))
        
        fit6 <- HoltWinters(dmsales[[2]])
        fit6fcast<-forecast(fit6,h=input$ahead)
        HW3Acc<-as.data.frame(accuracy(fit6fcast$mean,dmsales[[1]]$Sales))
        
        MAPE<-c(ArAcc$MAPE,LmAcc$MAPE,Hw1Acc$MAPE,HW2Acc$MAPE,HW3Acc$MAPE,STLFacc$MAPE)    #// All Models' Mape
        MAPE<-round(MAPE,digits=2)
        ModelForecast<-c("Arima","Linear Model","Basic Exponential Smoothing","Double Exponential Smoothing","Triple Exponential Smoothing","STLF")                               #// Model Names
        MinMape<-sort(MAPE)
        min_mape<-MinMape[1]
        Minmatch<-match(min_mape,MAPE) 
        var1 <- ModelForecast[Minmatch]
        
        var2<-switch(var1,
                     "Arima" = forecast(auto.arima(dmsales[[2]]),h=input$ahead),
                     "Linear Model"  = forecast(tslm(dmsales[[2]]~trend+season),h=input$ahead),
                     "Basic Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]],beta = FALSE,gamma = FALSE),h=input$ahead),
                     "Double Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]],gamma = FALSE),h=input$ahead),
                     "Triple Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]]),h=input$ahead),
                     "STLF" =stlf(dmsales[[2]],h=input$ahead,lambda=0)) 
        
        var2<-as.data.frame(var2)
      }
      else if(length(dmsales[[2]])<=24)
      {
        fit1 <- auto.arima(dmsales[[2]])
        fit1fcast<-forecast(fit1,h=input$ahead)
        ArAcc<-as.data.frame(accuracy(fit1fcast$mean,dmsales[[1]]$Sales))
        
        fit2 <- tslm(dmsales[[2]]~trend+season)
        fit2fcast<-forecast(fit2,h=input$ahead)
        LmAcc<-as.data.frame(accuracy(fit2fcast$mean,dmsales[[1]]$Sales))
        
        fit4 <- HoltWinters(dmsales[[2]], beta = FALSE, gamma = FALSE)
        fit4fcast<-forecast(fit4,h=input$ahead)
        Hw1Acc<-as.data.frame(accuracy(fit4fcast$mean,dmsales[[1]]$Sales))
        
        fit5 <- HoltWinters(dmsales[[2]], gamma = FALSE)
        fit5fcast<-forecast(fit5,h=input$ahead)
        HW2Acc<-as.data.frame(accuracy(fit5fcast$mean,dmsales[[1]]$Sales))
        
        MAPE<-c(ArAcc$MAPE,LmAcc$MAPE,Hw1Acc$MAPE,HW2Acc$MAPE)    #// All Models' Mape
        MAPE<-round(MAPE,digits=2)
        ModelForecast<-c("Arima","Linear Model","Basic Exponential Smoothing","Double Exponential Smoothing")                               #// Model Names
        MinMape<-sort(MAPE)
        min_mape<-MinMape[1]
        Minmatch<-match(min_mape,MAPE) 
        var1 <- ModelForecast[Minmatch]
        
        var2<-switch(var1,
                     "Arima" = forecast(auto.arima(dmsales[[2]]),h=input$ahead),
                     "Linear Model"  = forecast(tslm(dmsales[[2]]~trend+season),h=input$ahead),
                     "Basic Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]],beta = FALSE,gamma = FALSE),h=input$ahead),
                     "Double Exponential Smoothing" = forecast(HoltWinters(dmsales[[2]],gamma = FALSE),h=input$ahead)) 
        
        var2<-as.data.frame(var2)
      }
    
        y<-data.frame(seq(as.Date(dmsales[[3]]+32), by = "month", length.out = input$ahead))
        colnames(y)<-"Months"
        y<-paste(month(y$Months,label=TRUE,abbr=TRUE),"-",year(y$Months))
        bfcast<-as.data.frame(cbind(y,round(var2$`Point Forecast`,digits = 2),round(var2$`Lo 95`,digits = 2),round(var2$`Hi 95`,digits = 2)))
        names(bfcast)<-c("Month","Forecast","Lo-95","Hi-95")
        
        len.data<-length(dmsales[[6]]$Date)
        min.date<-dmsales[[6]]$Date[1]
        max.date<-max(dmsales[[6]]$Date)
        var_maxDate <- max(dmsales[[6]]$Date)
        
        mdate<-seq.Date(min.date,max.date,by="month")
        fordate<-data.frame(seq(var_maxDate+32, by = "month", length.out = input$ahead))
        colnames(fordate)<-c("Month")
        
        fplot<-plot_ly() %>%
          add_lines(x = mdate, y = dmsales[[2]],
                    color = I("black"), name = "observed") %>%
          add_lines(x = fordate$Month, y = var2$`Point Forecast`, color = I("blue"),name="Forecast",connectgaps = TRUE)%>%
          add_ribbons(x = fordate$Month, ymin = var2$`Lo 95`, ymax = var2$`Hi 95`,color = I("gray80"), name = "95% confidence")%>%
          layout(legend = list(orientation = 'h'))
        fplot
    }
   
    bflist<-list(min_mape,bfcast,var1,fplot)
    return(bflist)
  })

})
