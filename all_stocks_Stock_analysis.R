setwd("~/Nikita/S")

ImportPackages <-function() 
{
  pkg = c("data.table", "DT","shiny", "shinydashboard", "shinyjs", "shinyalert", 
          "shinyFiles","plotly", "xlsx", "readxl","openxlsx","dplyr","quantmod")
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
  {
    install.packages(new.pkg, dependencies = TRUE, repos = "http://cran.rstudio.com/")
  }
  sapply(pkg, require, character.only = TRUE)
  
}

ImportPackages()

options(DT.options = list(pageLength = 5, rownames = FALSE, scroller = TRUE, scrollX = TRUE, scrollY = TRUE))


Data_Prepration<- function()
{
  data1<-read.csv("all_stocks.csv")
  sel_sec<-data1['symbol']
  sector<-sel_sec[sel_sec!=""]
  sector[]<-lapply(sector,function(x) paste(x,"NS",sep="."))
  
  start <- as.Date("2020-01-01")
  end <- as.Date("2020-11-05")
  
  hist_data<-vector('list',length(sector))
  price_returns<-vector('list',length(sector))
  r_returns<-vector('list',length(sector))
  last_val<-vector('list',length(sector))
  per_value<-vector('list',length(sector))
  Low_val<-vector('list',length(sector))
  High_val<-vector('list',length(sector))
  Avg_price<-vector('list',length(sector))
  Ratios<-vector('list',length(sector))
  ma<-vector('list',length(sector))
  #hist_data[[1]][,4]
  
  for(i in 1:length(sector))
  {
    hist_data[[i]]<-getSymbols(sector[[i]],src="yahoo",from =start,to=end,auto.assign =FALSE)
    Avg_price[[i]]<-round(mean(hist_data[[i]][,4]),2)
    price_returns[[i]]<-round(ROC(hist_data[[i]][,4]),3)
  }
  
  
  price_returns<-lapply(price_returns, function(x) x[-1])
  std_devn<-lapply(price_returns, function(x) sd(x))
  m<-lapply(price_returns, function(x) mean(x))
  m<-lapply(m, function(x)  round(x,3))
  z=qnorm(.95)
  
  
  #hist_data[[1]][length(price_returns[[1]])+1,4]
  
  for(i in 1:length(sector))
  {
    last_val[[i]]<-hist_data[[i]][length(price_returns[[1]])+1,4]
    per_value[[i]]<-last_val[[i]]*std_devn[[i]]*z
    Low_val[[i]]<-round(last_val[[i]]-per_value[[i]],2)
    High_val[[i]]<-round(last_val[[i]]+per_value[[i]],2)
  }
  
  
  #hist_data[[i]][200:212,4]
  round(mean(hist_data[[i]][(length(price_returns[[1]])-8):(length(price_returns[[1]])+1),4]),2)
  
  for(i in 1:length(sector))
  {
    Ratios[[i]]<-getQuote(sector[[i]],what=yahooQF(c("Earnings/Share", "P/E Ratio", "Book Value")))
    #ma[[i]] <- rollmeanr(hist_data[[i]][,4],10,fill=NA,align ='right')
    ma[[i]]<-round(mean(hist_data[[i]][(length(price_returns[[1]])-8):(length(price_returns[[1]])+1),4]),2)
  }
  
  df<-do.call(rbind.data.frame,Ratios)
  df$Low_Val<-Low_val
  df$High_Val<-High_val
  df$M_Price<-Avg_price
  df$MO_10<-ma
  
  return(df)
}

Data_Prepration()