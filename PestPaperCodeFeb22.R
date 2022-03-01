# code required to generate figures for the Peach Potato Aphid paper



  # show peach potato aphid correlation between 
  require(ggplot2)
  require(dplyr)
  require(raster)
  require(stringr)
  require(lubridate)
  require(rgdal)
  
  # utility function that contains code to make maps and graphs
  source("//net/home/h05/hadnk/gitRepo/metoffice-science-dataviz/metoffice-science-dataviz/r-code/UtilityFunctions.R")
  
  inDir <- "/project/gisclim/Projects/Active/PlantPest/peachPotatoPaperFeb2022/"
  imageDir <- "/project/gisclim/Projects/Active/Pest_1920/Paper/"
  stYear=1980
  endYear=2018
  
  # read in the data files
  # was sent peach potato aphid emergence file by Lynda Alderson from Rothamstead which contains the percentiles of the 
  # flight time for Peach Potato Aphid for sites around the country, this has had Jan_feb temperatures add for each site
  peachTempSelect <- read.csv(paste0(inDir,"jfMinPeachEmergeStations.csv"))
  # read the January/February annual temperatures from the web
  yrs=1961:2018
  febDay=rep(28,length(yrs)) + leap_year(yrs)
  inUKMeanTemps= read.table("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/datasets/Tmean/date/UK.txt",skip=5,nrows=136,header=T)
  dfSel <- inUKMeanTemps[inUKMeanTemps$year %in% yrs,]
  dfJanFebMean = data.frame(year=yrs,jfTemp=(dfSel$jan*31 + dfSel$feb*febDay)/(31+febDay),jfTemp2=(dfSel$jan + dfSel$feb)/2)
  
  # read in the 2320 simulation of January-February temperature anomalies sent by Tyrone Dunbar on 17/07/2019
  jfUnseen <- as.numeric(unlist(read.table(paste0(inDir,"DePreSys_UK_Jan_Feb_anoms_20190717.txt"))) + 4.05)
 
  # read in r data files that contain the data to make maps of peach potato emergence for 1990 and 
  # 1990 plus the extra temperature that occurs as the 99th percentile of the unseen ensemble
  df1990 <- readRDS(paste0(inDir,"peach1990.rds"))
  df1990Unseen99 <- readRDS(paste0(inDir,"peach1990PlusUnseen99.rds"))
  
  # read in the file that has the potato field locations
  dfPot10km <- readRDS(paste0(inDir,"potatoFields.rds"))

  
  
  
  # read in the country outline for the maps
  shpUKCountries <- readOGR(inDir,"UkCountries")
  shpIre <- readOGR(inDir,"IRL_adm0") 

  
  # function return results from a linear regression
  getLinearReg <- function(X,Y){
    lmResult <- lm(formula = Y ~ X, na.action=na.exclude)
    intercept = coef(lmResult)[[1]]
    slope = coef(lmResult)[[2]]
    sumFit <- summary(lmResult)
    rsq = sumFit[8][[1]]
    rsq2 <- paste(round(rsq, 2))
    eq2 = paste0("y = ", round(slope, 1), "x +", round(intercept, 1), "\nR²  = ", rsq2)
    
    # return the intercept, correlation and slope
    return(c(intercept,slope,rsq,eq2))
  }
  
  # create a blank background
  backSet <- theme_bw() +  theme(legend.position="none") +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.border = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    theme(plot.title = element_text(size = 20,hjust = 0.5))

     
  # Regression between T_JF and 5th% flight
  # Produce site mean T_JF and 5th% flight by averaging all 13 sites for each year
  # Fit linear regression between T_JF (x-axis dependent variable) and 5th% flight (y-axis independent) 
  # site means only for the period 1980-2018 (when all 13 sites have data - except gap in Silwood) 
  # summarise the average of the 5th % of flight and the average JF temperature based on year
  
  peachSum <- peachTempSelect %>%
    group_by(year) %>%
    summarise(flight5th = mean(flightAdj),jfTemp=mean(jfTemp), n = n())
  peachSum <- as.data.frame(peachSum)
  
  
  outRegYrJF <- getLinearReg(peachSum$year-(stYear-1),peachSum$jfTemp)
  intercept = as.numeric(outRegYrJF[1])
  slope = as.numeric(outRegYrJF[2])
  
  peachSum$predJF=peachSum$year*slope + intercept
  peachSum$jfAnom = peachSum$jfTemp-peachSum$predJF
  peachSum$jfAnomPlus405 = peachSum$jfAnom+4.05

  jfTemps <- as.numeric(unlist(peachSum["jfTemp"]))
  perc5thFlight <- as.numeric(unlist(peachSum["flight5th"]))
  # get the regression equation to put on the graph
  outReg <- getLinearReg(jfTemps,perc5thFlight)[4]
  
  yAxTxt= c("135\n(May 15)","150\n(May 30)","165\n(Jun 14)","180\n(Jun 29)","195\n(Jul 14)","210\n(Jul 29)")
  p <- ggplot(data=peachSum) + xlim(c(-0.7,7.2)) 
  
  txtSize=5.9
  lineCol="#cccccc"
  for(i in 2:7) {
    p <- p + annotate("text",x=i,y=123,label=i,size=txtSize)
    p <- p + annotate("segment",x=i,xend=i,y=125,yend=210,color=lineCol)
    p <- p + annotate("segment",x=i,xend=i,y=125,yend=127,color="black")
  }
  
  cnt=1
  for (i in seq(135,210,by=15)) {
    p <-p + annotate("text",x=0.83,y=i,label=yAxTxt[cnt],hjust=1,size=txtSize,lineheight=0.8)
    p <- p + annotate("segment",x=0.86,xend=7,y=i,yend=i,color=lineCol)
    p <- p + annotate("segment",x=0.86,xend=1,y=i,yend=i,color="black")
    cnt=cnt+1
  }
  
  p <- p + annotate("segment",x=1,xend=1,y=125,yend=210)
  p <- p + annotate("segment",x=0.86,xend=7,y=127,yend=127)
  p <- p + geom_point(aes(x=jfTemp,y=flight5th),colour="#666666",size=2)  +
  geom_smooth(method = "lm", color="#666666",fill="#66666688",se = TRUE,aes(x=jfTemp,y=flight5th)) + 
  backSet

  p <- p + annotate("text",x=-0.2,y=170,label=expression(paste("Day of year of 5% ",italic("M. persicae "),"catch (",italic(M),DOY[5],")",sep="")),
                    angle=90,size=txtSize-1)
  p <- p + annotate("text",x=4,y=118,label=expression(paste("January-February mean temperature (", italic(M), T[J][F]," °C)",sep="")),size=txtSize-1)
  p <- p + annotate("rect",xmin=5.02,xmax=6.99,ymin=180.2,ymax=194.8,fill="white")
  p <- p + annotate("text",x=5.1,y=187.3,label=outReg,size=txtSize,hjust=0)
  
  outFile <- paste0(imageDir,"figure1.png")
  makeImage(outFile,300,10,7)
  multiplot(p,cols=1)
  dev.off()
  
  
  
  # get the linear regression of the average of all the UK 5th percent flights and Jan/Feb mean temperatures
  lmResult <- lm(formula = perc5thFlight~jfTemps, na.action=na.exclude)
  intercept = coef(lmResult)[[1]]
  slope = coef(lmResult)[[2]]
  
  peachSum$flight5thAdj= peachSum$jfAnomPlus405*slope + intercept

  rdCol <- "#db7474"
  blCol <- "#748fdb"
  
  minXPos =110
  
 
  
  lmResult <- lm(formula = dfJanFebMean$jfTemp~dfJanFebMean$year, na.action=na.exclude)
  intercept = coef(lmResult)[[1]]
  slope = coef(lmResult)[[2]]
  
  dfJanFebMean$predJF=(dfJanFebMean$year*slope) + intercept
  dfJanFebMean$jfAnom=dfJanFebMean$jfTemp-dfJanFebMean$predJF
  dfJanFebMean$jfAdj405=dfJanFebMean$jfAnom + 4.05
  dfJanFebMean$jfAdj346=dfJanFebMean$jfAnom + mean(dfJanFebMean$jfTemp)

    # get slope and intercept of equation predicting flight
  lmResult <- lm(formula = peachSum$flight5th~peachSum$jfTemp, na.action=na.exclude)
  intercept = coef(lmResult)[[1]]
  slope = coef(lmResult)[[2]]
  
  dfJanFebMean$flight5thRaw = dfJanFebMean$jfTemp*slope + intercept
  dfJanFebMean$flight5th346Mean = dfJanFebMean$jfAdj346*slope + intercept
  dfJanFebMean$flight5th405Mean = dfJanFebMean$jfAdj405*slope + intercept
  
  jfTypes <- c("flight5thRaw","flight5th346Mean","flight5th405Mean")
  jfTypes <- c("flight5th405Mean")
  
  # FOR HISTOGRAM COLOR IN EARLIER AND LATER BLUE AND THE REST GREY
  titStrs <- c("Raw unadjusted UK JF mean temperature","UK JF mean temperature adjusted to 3.46C (the actual 1961-2018 average)",
              "UK JF mean temperature adjusted to 4.05C (value of unseen adjusted 1961-2018 average)")
  cnt=1

  # this is set up to create different graphs but we are graphing observations that have been corrected for the 
  # climate change trend
  for (jfType in jfTypes) {
    
    p2 <- ggplot() + 
      geom_histogram(data=dfUnseen,aes(x=flight5th,fill = cut(flight5th, breaks=c(0,minAdj,maxAdj,400))),breaks=seq(minXPos,265),fill="#cccccc",col="#66666600",size=0.5)  +
      geom_histogram(data=dfUnseen[dfUnseen$flight5th<floor(min(dfJanFebMean[jfType])),],aes(x=flight5th,fill = cut(flight5th, breaks=c(0,minAdj,maxAdj,400))),breaks=seq(minXPos,265),fill="#bb4444",col="#66666600",size=0.5)  +
      geom_histogram(data=dfUnseen[dfUnseen$flight5th>ceiling(max(dfJanFebMean[jfType])),],aes(x=flight5th,fill = cut(flight5th, breaks=c(0,minAdj,maxAdj,400))),breaks=seq(minXPos,265),fill="#4444bb",col="#66666600",size=0.5)  +
      scale_fill_manual(values=c(rdCol,"#cccccc",blCol)) +
      geom_histogram(data=dfJanFebMean,aes_string(x=jfType),breaks=seq(minXPos,265), fill="#222222",col="#ffffff00")  +
      backSet
    
    cnt=cnt+1
    
    lineCol="black"
    
    for (i in seq(minXPos,260,by=10)) {
      p2 <- p2 + annotate("segment",x=i,xend=i,y=0,yend=-1.5,colour=lineCol)
      p2 <- p2 + annotate("text",x=i,y=-3,label=i,size=txtSize)
    }
    
    p2 <- p2 + annotate("segment",x=minXPos,xend=minXPos,y=0,yend=70,colour=lineCol)
    p2 <- p2 + annotate("segment",x=110,xend=265,y=0,yend=0,colour=lineCol)
    
    for (i in seq(0,70,by=10)) {
      p2 <- p2 + annotate("segment",x=minXPos,xend=(minXPos-2),y=i,yend=i,colour=lineCol)
      p2 <- p2 + annotate("text",x=(minXPos-2.5),y=i,label=i,size=txtSize,hjust=1)
    }
    
    p2 <- p2 + annotate("text",label=expression(paste("Estimated day of year of 5% ",italic("M. persicae "),"catch (",italic(M),DOY[5],")",sep="")),
                        x=180,y=-7,size=txtSize-1,hjust=0.5,parse=T)
    p2 <- p2 + annotate("text",label="Count",x=minXPos-10,y=35,size=txtSize-1,angle = 90)

    siteFile <- paste0(imageDir,"figure3.png")
    makeImage(siteFile,300,11,7)
    multiplot(p2,cols=1)
    dev.off()

  }
  
  
  #---------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------
  # Create figure4 that shows distribution of emergence of pest compared to observational years
  #---------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------
  
  
  # using the unseen ensemble to create distribution of possible outcomes
  dfUnseen <- data.frame(jfTemp=jfUnseen)
  dfUnseen$flight5th=NA
  
  # CHANGE THE VALUES FROM ANOMALIES TO ABSOLUTE VALUES USING THE BASELINE TEMP OF 4.05C
  dfUnseen$flight5th=dfUnseen$jfTemp*slope+intercept
  minAdj <- floor(min(dfUnseen$flight5th))
  maxAdj <- round(max(dfUnseen$flight5th))
  
  
  dfUk <- dfJanFebMean[dfJanFebMean$year>=1960,]
  dfUk$flight5th405Mean = dfUk$jfAdj405*slope + intercept
  
  # 95 % probability
  jfTempThresh=quantile(sort(dfUk$jfAdj),probs=c(0.95))
  jfTempThresh=quantile(dfUnseen$jfTemp,probs=0.85)
  
  extremeTemps <- sort(dfUnseen[dfUnseen$jfTemp>jfTempThresh,]$jfTemp)
  memCount <- length(extremeTemps)
  extremeFlights <- sort(dfUnseen$flight5th)[1:memCount]
  
  percMore <- (memCount/ 2320) * 100
  
  jfTempDf <- data.frame(
    jfTemp = extremeTemps
  )
  
  flight5th=dfUk$flight5th405Mean
  jfTemp=dfUk$jfAdj405
  
  jfTemps <- as.numeric(unlist(peachSum["jfTemp"]))
  perc5thFlight <- as.numeric(unlist(peachSum["flight5th"]))
  lmJFTempFlight5th <- lm(formula = perc5thFlight~jfTemps, na.action=na.exclude)
  
  percVals <- seq(percMore/memCount,percMore,by=percMore/memCount)
  
  modCount <- 2320
  model2 <- stats::lm(flight5th ~ jfTemp)
  
  # get the linear regression of the average of all the UK 5th percent flights and Jan/Feb mean tmperatures
  lmResult <- lm(formula = flight5th~jfTemp, na.action=na.exclude)
  
  new2 = data.frame(jfTemps=rev(extremeTemps))
  confVals <- stats::predict(lmJFTempFlight5th, new2, interval = "confidence")
  
  lowVals <- as.numeric(confVals[,2])
  highVals <- as.numeric(confVals[,3])
  
  percAdj <- 1/(percVals/100)
  percAdj <- percVals * -1
  
  dfExtremes <- data.frame(perc = percAdj,mean=extremeFlights,low=lowVals,high=highVals)
  perc005 <- quantile(dfUnseen$flight5th,probs=0.005)
  #perc15 <- quantile(dfUnseen$flight5th,probs=0.15)
  
  dfExtremes <- dfExtremes[dfExtremes$mean > perc005,]
  
  
  
  rev_sqrt_trans <- function() {
    scales::trans_new(
      name = "rev_sqrt", 
      transform = function(x) -sqrt(abs(x)), 
      inverse = function(x) x^2)
  }
  
  #dfExtremes <- dfExtremes[dfExtremes$perc> -15,]
  
  #tn <- trans_new(name="isq", transform = function(x) -sqrt(abs(x)),inverse = function(x) x^2)
  p <- ggplot(data=dfExtremes) + xlim(c(-18,1.6)) +
    geom_point(data=data.frame(x=-25,y=120),aes(x=x,y=y),size=0,color="white") +
    geom_ribbon(aes(ymin=low, ymax=high, x=perc, fill = "band"), fill="#cccccc") +
    geom_line(aes(x=perc,y=mean),color="black") +
    geom_line(aes(x=perc,y=low),color="#888888") +
    geom_line(aes(x=perc,y=high),color="#888888") #+
  #coord_cartesian(xlim=c(-15,0))
  
  p <- p + scale_y_reverse()
  
  allFlights <- sort(dfUnseen$flight5th)
  
  flight1in5 <- quantile(allFlights,prob=0.25)
  flight1in10 <- quantile(allFlights,prob=0.1)
  flight1in20 <- quantile(allFlights,prob=0.05)
  flight1in100 <- quantile(allFlights,prob=0.01)
  flight1in1000 <- quantile(allFlights,prob=0.001)
  
  xLabPos= -20
  xSt = -12.5
  xEnd= -0.5
  
  jf1990 <- max(dfUk$jfAdj405)
  
  highJFTemps <- dfUk[dfUk$flight5th405Mean < max(dfExtremes$mean),]
  obs1990 <-  jf1990 * slope + intercept
  
  yLow= 160
  
  numY <- min(dfExtremes$low) -2
  labY <- min(dfExtremes$low) -4
  numY <- yLow +2
  labY <- yLow +4
  
  xSt= ((length(allFlights[allFlights<obs1990])/2320) *100) * -1
  
  p <- p + annotate("segment",x=min(percAdj),xend=xSt,y=obs1990,yend=obs1990,color="#00000088")
  p <- p + annotate("segment",x=xSt,xend=xSt,y=obs1990,yend=yLow,color="#00000088")
  p <- p + annotate("segment",x=-1,xend=-1,y=flight1in100,yend=yLow,color="#00000088")
  p <- p + annotate("segment",x=min(percAdj),xend=-1,y=flight1in100,yend=flight1in100,color="#00000088")
  
  # because I can't get dashed line to look right I am doing this part manually
  
  for (xP in seq(flight1in100,yLow,by=1)) {
    ptCol="white"
    if (xP<141.8) ptCol = "#cccccc"
    p <- p + annotate("point",x=-1,y=xP,color=ptCol,shape=15,size=1)
  }
  
  for (xP in seq(min(percAdj),-1,by=0.5)) {
    ptCol="white"
    if (xP > -3) ptCol = "#cccccc"
    p <- p + annotate("point",x=xP,y=flight1in100,color=ptCol,shape=15,size=1)
  }
  p <- p + annotate("text",x=xSt*0.99,y=obs1990*1.002,label="1990",hjust=0, vjust=1,size=6)
  
  p <- p + annotate("text",x=-11.8,y=yLow+3,label="More likely",hjust=0,size=txtSize-1,color="#444444")
  p <- p + annotate("text",x=-7.5,y=yLow+3,label="Chance of event",hjust=0.5,size=txtSize,color="#444444",fontface=2)
  p <- p + annotate("text",x=-3.2,y=yLow+3,label="Less likely",hjust=1,size=txtSize-1,color="#444444")
  
  
  yArr= labY
  d=data.frame(x=c(-12,-3), y=c(yLow+3,yLow+3), vx=c(-15,-0.5), vy=c(yLow+3,yLow+3))
  p <- p + geom_segment(data=d, mapping=aes(x=x, y=y, xend=vx, yend=vy), arrow=arrow(), size=1.2, color="#888888") 
  
  labStr <- rev(c("1%","5%","10%","15%"))
  
  cnt=1
  
  tempPosArr <- quantile(dfUnseen$flight5th,probs=c(0.15,0.1,0.05,0.01)) * -1
  
  for (perc in c(15,10,5,1)) {
    
    xPos=perc
    p <- p + annotate("text",x=xPos * -1,y=yLow+1.5,label=labStr[cnt],hjust=0.5,size=txtSize)
    #p <- p + annotate("segment",x=xPos * -1,xend=xPos * -1,y=min(dfExtremes$low),yend=numY+1,color="#888888")
    p <- p + annotate("segment",x=xPos * -1,xend=xPos * -1,y=yLow+1.5,yend=yLow+1.5,color=lineCol)
    
    #if (cnt>1) p <- p + annotate("segment",x=xPos * -1,xend=xPos * -1,y=tempPosArr[cnt],yend=numY+1,color="#888888")
    
    cnt = cnt+1
  }
  
  #add some temperature axes
  
  
  flight50th = quantile(dfUnseen$flight5th,probs=0.15) #* -1
  
  flight50th= 155
  
  leftYAxisXPos = max(percVals) * -1
  
  lineCol="black"
  
  p <- p + annotate("segment",x=leftYAxisXPos,xend=leftYAxisXPos,y=125,yend=yLow,color=lineCol)
  p <- p + annotate("segment",x=-0.5,xend=-0.5,y=125,yend=yLow,color=lineCol)
  #p <- p + annotate("segment",x=leftYAxisXPos,xend=min(percVals),y=min(dfExtremes$low),yend=min(dfExtremes$low),color="#888888")
  p <- p + annotate("segment",x=leftYAxisXPos,xend=-0.5,y=yLow,yend=yLow,color=lineCol)
  
  
  
  cnt=0
  for (j in seq(flight50th+5,flight50th-30,by=-5)) {
    yPos=j
    dateVal=format(as.Date(j, origin = "2016-01-01"),"%b %d")
    dateStr = paste0(j,"\n(",dateVal,")")
    #if (cnt==0) dateStr = paste0(dateVal, "\n average DoY5%")
    
    p <- p + annotate("text",x=-15.6,y=yPos,label=dateStr,hjust=1,size=txtSize, color="#444444",lineheight = .85)
    p <- p + annotate("segment",x=leftYAxisXPos,xend=-15.4,y=yPos,yend=yPos,color=lineCol)
    
    cnt=cnt+10
  }
  
  slope= -12.52
  intercept=222.09
  
  labStr=c("5.0","5.5","6.0","6.5","7.0","7.5")
  
  iCnt=1
  for (i in seq(5,7.5,by=0.5)) {
    yPos=(i*slope + intercept) 
    p <- p + annotate("text",x=0,y=yPos,label=labStr[iCnt],hjust=0,size=txtSize, color="#444444")
    p <- p + annotate("segment",x=-0.5,xend=-0.1,y=yPos,yend=yPos,color=lineCol)
    iCnt=iCnt+1
  }
  
  #ylab(expression(doy[5])) +
  #xlab(expression(paste( T[J][F]," (°C)",sep=""))) +
  
  for(r in 1:nrow(highJFTemps)) {
    curRow <- highJFTemps[r,]
    curObs <- curRow$flight5th405Mean
    curYear<- curRow$year
    
    xYear= ((length(allFlights[allFlights<curObs])/2320) *100) * -1
    
    
    adjY=0.2
    
    if(curYear !=1990) {
      
      if(curYear==1974) adjY=1
      
      p <- p + annotate("point",x=xYear,y=curObs,color="black",shape=4,size=3)
      p <- p + annotate("text",x=xYear,y=curObs+adjY,color="black",label=curYear,size=4,hjust=-0.1)
    } 
  }
  
  
  p <- p + annotate("text",label=expression(paste("January-February mean temperature (", italic(M), T[J][F]," °C)",sep="")),
                    color="#444444",x=1,y=142.5,size=txtSize,angle=90)
  p <- p + annotate("text",
                    label=expression(paste("Estimated day of year of 5% ",italic("M. persicae "),"catch (",italic(M),DOY[5],")",sep="")),
                    color="#444444",x=-17.5,y=142.5,size=txtSize,angle=90)
  
  p <- p +  backSet
  
  siteFile <- paste0(imageDir,"figure4.png")
  makeImage(siteFile,300,13,10)
  multiplot(p,cols=1)
  dev.off()
  


  #---------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------
  # Create figure 5 that shows maps of emergence over the UK for 1990 and for the 99% of the 
  # the unseen ensemble
  #---------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------
  
  
  incols <- c("#fcae91","#fb6a4a","#b3393d","#ffd019","#fdb800","#d08c00","#bdd7e7","#6baed6","#2171b5","#99d8c9","#238b45")
  brks <- c(84,100,110,120,130,140,150,160,170,180,200,236)+1

  jfTempDir <- "/project/gisclim/Projects/Active/PlantPest/peachPotWin/emergence1km/jfTemp/"
  ras1990 <- raster(paste0(jfTempDir,"jf_1990.tif"))
  peach1990 <- ras1990*slope + intercept
  peach1990Unseen99  <- (ras1990+1.25)*slope + intercept
  
  # These are saved out as it uses an external gdal command
  #df1990 <- dfFromRas(peach1990)
  #df1990Unseen99 <- dfFromRas(peach1990Unseen99)
  #saveRDS(df1990,paste0(inDir,"peach1990.rds"))
  #saveRDS(df1990Unseen99,paste0(inDir,"peach1990PlusUnseen99.rds"))
  
  
  df1990$zcut <- cut(df1990$z,breaks=brks,right=FALSE)
  df1990Unseen99$zcut <- cut(df1990Unseen99$z,breaks=brks,right=FALSE)
  
  natGridString <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
  
  # for the second map of the UK we will shift it 700km so it can display both of them
  shiftX=8e5
  repIre <- fortify(spTransform(shpIre, natGridString))
  dfShp <- fortify(shpUKCountries)
  dfShpNonEng <- dfShp[dfShp$id !=0,]
  dfShpEng <- dfShp[dfShp$id ==0,]

  repIre2 <- repIre
  dfShpNonEng2 <- dfShpNonEng
  dfShpEng2 <- dfShpEng
  repIre2$long = repIre2$long +shiftX
  dfShpNonEng2$long = dfShpNonEng2$long +shiftX
  dfShpEng2$long = dfShpEng2$long +shiftX
  
  dfPot10km1Perc <- dfPot10km[dfPot10km$z>0.01,]
  dfPot10km5Perc <- dfPot10km[dfPot10km$z>0.05,]
  

  xlims <- c(0,680000)
  xlims <- c(0,14e5)
  ylims <- c(0,1.07e6)
  
  
  p2 <- ggplot() + ylim(0,1.15e6) +
    geom_tile(data=df1990Unseen99,inherit.aes = FALSE, aes(x=x+shiftX, y=y, fill=zcut), alpha=1) +
    scale_fill_manual(values= incols) +
    geom_tile(data=dfPot10km1Perc,inherit.aes = FALSE, aes(x=x+shiftX, y=y),fill="#ffffff00",color="#222222", alpha=0,size=0.3) +
    geom_tile(data=dfPot10km5Perc,inherit.aes = FALSE, aes(x=x+shiftX, y=y),fill="#ffffff00",color="#222222", alpha=0,size=0.75) 
  
  p2  <- p2 + geom_map(data=dfShpNonEng2, map=dfShpNonEng2,
                       aes(x=long, y=lat, map_id=id),
                       color="#666666", fill="#ffffff00", size=0.25) +
    geom_map(data=fortify(dfShpEng2), map=fortify(dfShpEng2),
             aes(x=long, y=lat, map_id=id),
             color="#666666", fill="#ffffff00", size=0.25) + #scale_fill_manual(values = cols) +
    geom_map(data=repIre2, map=repIre2,
             aes(x=long, y=lat, map_id=id),
             color="#666666", fill="#ffffff00", size=0.25) + #scale_fill_manual(values = cols) +
    coord_cartesian(xlim = xlims,ylim=ylims) +
    backSet
  
  p1 <- p2 +
    annotate("rect",xmin=5e5,xmax=8e5,ymin=0,ymax=1e6,fill="white",color=NA) +
    
    geom_tile(data=df1990,inherit.aes = FALSE, aes(x=x, y=y, fill=zcut), alpha=1) +
    scale_fill_manual(values= incols) +
    geom_tile(data=dfPot10km1Perc,inherit.aes = FALSE, aes(x=x, y=y),fill="#ffffff00",color="#222222", alpha=0,size=0.25) + 
    geom_tile(data=dfPot10km5Perc,inherit.aes = FALSE, aes(x=x, y=y),fill="#ffffff00",color="#222222", alpha=0,size=0.75) 
  
  p1  <- p1 + geom_map(data=dfShpNonEng, map=dfShpNonEng,
                       aes(x=long, y=lat, map_id=id),
                       color="#666666", fill="#ffffff00", size=0.25) +
    geom_map(data=fortify(dfShpEng), map=fortify(dfShpEng),
             aes(x=long, y=lat, map_id=id),
             color="#666666", fill="#ffffff00", size=0.25) + #scale_fill_manual(values = cols) +
    geom_map(data=repIre, map=repIre,
             aes(x=long, y=lat, map_id=id),
             color="#666666", fill="#ffffff00", size=0.25) + #scale_fill_manual(values = cols) +
    #coord_cartesian(xlim = xlims, ylim = ylims) +
    backSet

  
  brks <- c(84,100,110,120,130,140,150,160,170,180,200,236)+1
  
  textDate1 <- format(strptime(head(brks+1,length(brks)-1), format="%j"), format="%b %d")
  textDate2 <- format(strptime(tail(brks,length(brks)-1), format="%j"), format="%b %d")
  textDateComb <- paste(textDate1,"to",textDate2)
  
  textDay1 <- head(brks,length(brks)-1)
  textDay2 <- tail(brks-1,length(brks)-1)
  textDayComb <- paste(textDay1,"to",textDay2)
  #textDayComb <- rev(textDayComb[1:10])
  #p3 <- ggplot() + xlim(c(-1,1)) + ylim(c(-3,13))
  
  p3 <- p1
  
  p3 <- p3 + annotate("text",x=5.5e5,y=1e6,label="day of year",hjust=1,size=5.5,fontface=2)
  p3 <- p3 + annotate("text",x=6.5e5,y=1e6,label="date",hjust=0,size=5.5,fontface=2)
  
  
  boxSize=4e4
  textLeft=5.5e5
  textRight=6.3e5
  
  for (i in seq(1,length(textDateComb)-1)) {
    yPos =1e6-(i*boxSize)
    p3 <- p3 + annotate("text",x=textLeft,y=yPos,label=textDayComb[i],hjust=1,size=4.5)
    p3 <- p3 + annotate("text",x=textRight,y=yPos,label=textDateComb[i],hjust=0,size=4.5)
    p3 <- p3 + annotate("rect",xmin=textLeft+1e4,xmax=textRight-1e4,ymin=1.02e6-(i*boxSize),ymax=0.98e6-(i*boxSize),fill=incols[i])
  }
  
  
  
  labPot = c("> 1% potato","> 5% potato")
  lineWidth = c(0.25,0.75)
  
  for (i in 1:2) {
    j=i+10
    yPos =0.97e6-(j*boxSize)
    p3 <- p3 + annotate("rect",xmin=textLeft+1e4,xmax=textRight-1e4,ymin=0.99e6-(j*boxSize),ymax=0.95e6-(j*boxSize),color="#222222",fill=NA,size=lineWidth[i])
    p3 <- p3 + annotate("text",x=textRight,y=yPos,label=labPot[i],hjust=0,size=4.5)
  }

  p3 <- p3 + annotate("rect",xmin=0,xmax=14e5,ymin=1.02e6,ymax=1.15e6,fill="white",color=NA)
  p3 <- p3 + annotate("text",x=0,y=1.07e6,label="a) Aphid earliest flight estimated based on the observed",size=5,fontface=2,hjust=0)
  p3 <- p3 + annotate("text",x=3e4,y=1.04e6,label="warmest January-February temperatures (1990)",size=5,fontface=2,hjust=0)
  p3 <- p3 + annotate("text",x=7.5e5,y=1.07e6,label="a) Aphid earliest flight estimated based on the warmest",size=5,fontface=2,hjust=0)
  p3 <- p3 + annotate("text",x=7.5e5 +3e4,y=1.04e6,label="1% of modelled January-February temperatures (1990)",size=5,fontface=2,hjust=0)
  
  
  p3 <- p3 + backSet

  siteFile2 <- paste0(imageDir,"figure5.png")
  makeImage(siteFile2,300,14,11.5)
  multiplot(p3,cols=1)
  dev.off()
