
library("ASAPplots")
library("dplyr")
library("ggplot2")

# to do: 
# add GOM haddock data
# 
# calculate CCZ using the DLMTool method
# calulate Sinclair Z for 4 and 5 years, and avg CI for 4 and 5 years.  Maybe plot the Spring Z vs CI from the previous year
# calculate Sinclair Z using the fishery CAA
# PLot relative CI for stocks by region and year, calculate and overall average for each region
# Plot cohort CI and Z to see some of the extremes


{
user <- "MAC"  # Use ("MAC" or "PC") - there are different ways to access directories \ or / 

if(user=="MAC") 
{
  #source(".//Rcode//Groundfish_functions.R")
  decoder <- read.csv(".//ADIOS_data//file_decoder.csv")
  so_use <- read.csv(".//ADIOS_data//survey_options_use.csv", stringsAsFactors = FALSE)
  #so_use <- so_use[so_use$survey=="NMFS fall BTS" | so_use$survey=="NMFS spring BTS",]
  
  caa.dat <- read.table(".//ADIOS_data//NEFSC_CAA.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  ctot.dat <-read.table(".//ADIOS_data//NEFSC_total_catch.txt",sep="\t",header=TRUE,stringsAsFactors = FALSE)
  stot.dat <-read.table(".//ADIOS_data//NEFSC_annual_survey.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  gbyt.sa.dat <- read.table(".//ADIOS_data//GB_yellowtail_SA_estimates.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  
 
  #cs.dat$CI[cs.dat$stock=='herring'] = cs.dat$catch[cs.dat$stock=='herring']/cs.dat$n_tow[cs.dat$stock=='herring'] # for herring use N / tow
}
  
else if(user=="PC")
{
  source(".\\Rcode\\Groundfish_functions.R")
  decoder <- read.csv(".\\ADIOS_data\\file_decoder.csv")
  so_use <- read.csv(".\\ADIOS_data\\survey_options_use.csv", stringsAsFactors = FALSE)
  #so_use <- so_use[so_use$survey=="NMFS fall BTS" | so_use$survey=="NMFS spring BTS",]
  caa.dat <- read.table(".\\ADIOS_data\\NEFSC_CAA.txt",sep="\t", stringsAsFactors = FALSE)
  ctot.dat <-read.table(".\\ADIOS_data\\NEFSC_total_catch.txt",sep="\t", stringsAsFactors = FALSE)
  stot.dat <-read.table(".\\ADIOS_data\\NEFSC_annual_survey.txt",sep="\t", stringsAsFactors = FALSE)
  gbyt.sa.dat <- read.table(".\\ADIOS_data\\GB_yellowtail_SA_estimates.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  
}  
  
  cs.dat <- merge(x=stot.dat,y=ctot.dat,by=c("stock","year"),all=TRUE)
  cs.dat$CIW <- cs.dat$catch/cs.dat$kg_tow  # calculate catch per index using kg / tow
  cs.dat$CIN <- cs.dat$catch/cs.dat$n_tow  # calculate catch per index using kg / tow
  
  
}




nstocks <- length(decoder$Short.Name)
res <- list()
resdf <- data.frame()
#for (istock in 1:1){
for (istock in 1:nstocks){
  cur.stock <- decoder$Short.Name[istock]
  
  if(user=="PC")dat <- read.csv(paste0(".\\ADIOS_data\\", decoder$ADIOS.name[istock], ".csv"))
  else if(user=="MAC") dat <- read.csv(paste0(".//ADIOS_data//", decoder$ADIOS.name[istock], ".csv"))
  dat <- dat[dat$SURVEY=="NMFS fall BTS" |dat$SURVEY=="NMFS spring BTS",]  # only use the NMFS surveys
  surveys <- unique(dat$SURVEY)
  nsurveys <- length(surveys)
  res[[istock]] <- list()
  res[[istock]]$stock <- decoder$Short.Name[istock]
  res[[istock]]$region <- decoder$region[istock]
  res[[istock]]$surveys <- surveys
  res[[istock]]$z <- list()
 
  caa <- caa.dat %>% filter(stock==decoder$Short.Name[istock], 
                            year >= 1978, age >0)
  if(dim(caa)[1]==0)
  {
    caa.df <- data.frame(stock = decoder$Short.Name[istock],
                                     #survey = as.character(surveys[isurvey]),
                                     year = 2000,
                                     CAA_Z = NA,
                                     low90_CAA = NA,
                                     high90_CAA = NA)
  }
  else
  {
  min.year.caa <- min(caa$year,na.rm=TRUE)
  max.year.caa <- max(caa$year,na.rm=TRUE)
  min.age.caa<-min(caa$age, na.rm=TRUE)
  max.age.caa<-max(caa$age, na.rm=TRUE)
  
  
  caa.mat <- matrix(NA, nrow = (max.year.caa - min.year.caa + 1), 
                    ncol = (max.age.caa - min.age.caa + 1), 
                dimnames = list(seq(min.year.caa, max.year.caa), 
                                seq(min.age.caa, max.age.caa)))
  
  for (cc in 1:length(caa[,1]))
  {
    myrow <- caa$year[cc] - min.year.caa + 1
    mycol <- caa$age[cc] - min.age.caa + 1
    caa.mat[myrow, mycol] <- as.numeric(caa$CAA[cc])
  }
  
  myres.sin.caa3 <- calc_Sinclair_Z_dynfa(caa.mat,3)
  myres.sin.caa4 <- calc_Sinclair_Z_dynfa(caa.mat,4)
  myres.sin.caa5 <- calc_Sinclair_Z_dynfa(caa.mat,5)
  
  caa.df3 <- data.frame(stock = decoder$Short.Name[istock],
                       #survey = as.character(surveys[isurvey]),
                       #Year = rep(myres.sin.caa3$plot.year,1),
                       year = rep(myres.sin.caa3$plot.year,1),
                       CAA_Z3 = myres.sin.caa3$est.Sinclair.Z[,1],
                       low90_CAA_Z3 = myres.sin.caa3$est.Sinclair.Z[,2],
                       high90_CAAZ3 = myres.sin.caa3$est.Sinclair.Z[,3])
  
  caa.df4 <- data.frame(stock = decoder$Short.Name[istock],
                        #survey = as.character(surveys[isurvey]),
                        #Year = rep(myres.sin.caa4$plot.year-0.5,1),
                        year = rep(myres.sin.caa4$plot.year,1),
                        CAA_Z4 = myres.sin.caa4$est.Sinclair.Z[,1],
                        low90_CAA_Z4 = myres.sin.caa4$est.Sinclair.Z[,2],
                        high90_CAAZ4 = myres.sin.caa4$est.Sinclair.Z[,3])
  
  caa.df5 <- data.frame(stock = decoder$Short.Name[istock],
                        #survey = as.character(surveys[isurvey]),
                        #Year = rep(myres.sin.caa5$plot.year,1),
                        year = rep(myres.sin.caa5$plot.year,1),
                        CAA_Z5 = myres.sin.caa5$est.Sinclair.Z[,1],
                        low90_CAA_Z5 = myres.sin.caa5$est.Sinclair.Z[,2],
                        high90_CAAZ5 = myres.sin.caa5$est.Sinclair.Z[,3])
 
   #caa.df = merge(caa.df3,caa.df4,by=c("stock","Year","year"),all=TRUE)
   #caa.df = merge(caa.df,caa.df5,by=c("stock","Year","year"),all=TRUE)
  caa.df = merge(caa.df3,caa.df4,by=c("stock","year"),all=TRUE)
  caa.df = merge(caa.df,caa.df5,by=c("stock","year"),all=TRUE)
   
  
  }
  # loop over the surveys
  for (isurvey in 1:nsurveys)
  {
    # this is for the CI calculations
    cs.temp <- filter(cs.dat, stock == decoder$Short.Name[istock], 
                      survey == surveys[isurvey], year >=1978)

    
    if(decoder$Short.Name[istock]=='herring')
    {
      cs.temp <- filter(cs.dat, stock == decoder$Short.Name[istock], 
                        survey == surveys[isurvey], year >=1988)
    }
    
    # below accounts for empty catch / survey dataframes for some of the stocks
    cs.dim <-dim(cs.temp)
    if(cs.dim[1]==0){
      cs.temp <- data.frame(matrix(NA,nrow=1,ncol=cs.dim[2]))
      colnames(cs.temp) <- colnames(cs.dat)
      cs.temp$stock <- decoder$Short.Name[istock]
      cs.temp$survey <- surveys[isurvey]
      #cs.temp$Year <- 2000 #max(dat$YEAR,na.rm=TRUE)
      cs.temp$year <- 2000
      cs.temp$relCIN <- NA
      cs.temp$relCIW <- NA
      cs.temp$CIN_3yr <- NA
      cs.temp$CIW_3yr <- NA
      cs.temp$relCIN_3yr <- NA
      cs.temp$relCIW_3yr <- NA
      cs.temp$CIN_4yr <- NA
      cs.temp$relCIN_4yr <- NA
      cs.temp$CIW_4yr <- NA
      cs.temp$relCIW_4yr <- NA
      cs.temp$CIN_5yr <- NA
      cs.temp$relCIN_5yr <- NA
      cs.temp$CIW_5yr <- NA
      cs.temp$relCIW_5yr <- NA
      
      cs.temp.new <- cs.temp
    }
    
    else
    {
      
      cs.temp35 <- cs.temp
      cs.temp35$relCIN <- cs.temp$CIN/max(cs.temp$CIN,na.rm=TRUE)
      cs.temp35$relCIW <- cs.temp$CIW/max(cs.temp$CIW,na.rm=TRUE)
      Avg_CIN <- move.avg.2(cbind(cs.temp$year,cs.temp$CIN),3)
      cs.temp35$CIN_3yr <- as.numeric(Avg_CIN[,2])
      Avg_CIW <- move.avg.2(cbind(cs.temp$year,cs.temp$CIW),3)
      cs.temp35$CIW_3yr <- as.numeric(Avg_CIW[,2])
      cs.temp35$relCIN_3yr <- cs.temp35$CIN_3yr/max(cs.temp35$CIN_3yr,na.rm=TRUE)  # 3 year moving average
      cs.temp35$relCIW_3yr <- cs.temp35$CIW_3yr/max(cs.temp35$CIW_3yr,na.rm=TRUE)  # 3 year moving average
      
      
      Avg_CIN <- move.avg.2(cbind(cs.temp$year,cs.temp$CIN),5)
      cs.temp35$CI_5yr <- as.numeric(Avg_CIN[,2])
      cs.temp35$relCI_5yr <- cs.temp35$CI_5yr/max(cs.temp35$CI_5yr,na.rm=TRUE)  # 5 year moving average
      Avg_CIW <- move.avg.2(cbind(cs.temp$year,cs.temp$CIW),5)
      cs.temp35$CIW_5yr <- as.numeric(Avg_CIW[,2])
      cs.temp35$relCIW_5yr <- cs.temp35$CIW_5yr/max(cs.temp35$CIW_5yr,na.rm=TRUE)  # 5 year moving average
      
      
      cs.temp4<- cs.temp[,1:3]
      Avg_CIN <- move.avg.2(cbind(cs.temp$year,cs.temp$CIN),4)
      cs.temp4$year <- cs.temp$year+0.5
      cs.temp4$CIN_4yr <- as.numeric(Avg_CIN[,2])
      cs.temp4$relCIN_4yr <- cs.temp4$CIN_4yr/max(cs.temp4$CIN_4yr,na.rm=TRUE)  # 4 year moving average
      Avg_CIW <- move.avg.2(cbind(cs.temp$year,cs.temp$CIW),4)
      cs.temp4$year <- cs.temp$year+0.5
      cs.temp4$CIW_4yr <- as.numeric(Avg_CIW[,2])
      cs.temp4$relCIW_4yr <- cs.temp4$CIW_4yr/max(cs.temp4$CIW_4yr,na.rm=TRUE)  # 4 year moving average
      
      cs.temp.new <- merge(cs.temp35,cs.temp4,by=c("stock","survey","year"),all=TRUE)

    }
    
    
    # this is for the Z calculations
    myrow <- filter(so_use, stock == decoder$Short.Name[istock], survey == surveys[isurvey]) %>%
      select(rowID) %>%
      as.numeric(.)
    if (so_use$usesurvey[myrow] == TRUE){
      sdat <- dat %>%
        filter(SURVEY == surveys[isurvey]) %>%
        filter(AGE %in% seq(so_use$startage[myrow], so_use$endage[myrow]))
      all.sdat <-  dat %>%
        filter(SURVEY == surveys[isurvey])  %>%
        filter(AGE > 0)
      
      minyear <- min(sdat$YEAR)
      maxyear <- max(sdat$YEAR)
      minage.all<-max(1,min(all.sdat$AGE))
      minage <- min(sdat$AGE)
      maxage <- max(sdat$AGE)
      
      all.sdat <- all.sdat  %>%
        filter(AGE %in% seq(minage.all, maxage))

      mat <- matrix(NA, nrow = (maxyear - minyear + 1), ncol = (maxage - minage + 1), 
                    dimnames = list(seq(minyear, maxyear), seq(minage, maxage)))
      all.mat <-matrix(NA, nrow = (maxyear - minyear + 1), ncol = (maxage - minage.all + 1), 
                       dimnames = list(seq(minyear, maxyear), seq(minage.all, maxage)))
      
      for (j in 1:length(sdat[,1])){
        myrow <- sdat$YEAR[j] - minyear + 1
        mycol <- sdat$AGE[j] - minage + 1
        mat[myrow, mycol] <- sdat$NO_AT_AGE[j]
      }
      
      for(jj in 1:length(all.sdat[,1]))
      {
        myrow <- all.sdat$YEAR[jj] - minyear + 1
        mycol.all <- all.sdat$AGE[jj] - minage.all + 1
        all.mat[myrow, mycol.all] <- all.sdat$NO_AT_AGE[jj]
      }
      
      myres.sin3 <- calc_Sinclair_Zmod(mat,3)
      myres.sin4 <- calc_Sinclair_Zmod(mat,4)
      myres.sin5 <- calc_Sinclair_Zmod(mat,5)
      
      myres.coh <- calc_cohort_Z(mat,8)
      myres.sin3.dynfa <- calc_Sinclair_Z_dynfa(all.mat,3)
      myres.sin4.dynfa <- calc_Sinclair_Z_dynfa(all.mat,4)
      myres.sin5.dynfa <- calc_Sinclair_Z_dynfa(all.mat,5)
      
      if(dim(caa)[1]==0)
      {
        coh.ci <-data.frame(year=2000,cohort_CI=NA,
                            stock= decoder$Short.Name[istock],
                            survey=as.character(surveys[isurvey]))
      }
      else
      {
        #coh.ci <- calc_cohort_CI(all.mat,caa.mat,8)
        coh.ci <- calc_cohort_CI(mat,caa.mat,8)
        coh.ci$stock <- decoder$Short.Name[istock]
        coh.ci$survey <-as.character(surveys[isurvey])
      }
      
      res[[istock]]$z.sin[[isurvey]] <- myres.sin3
      res[[istock]]$z.coh[[isurvey]] <- myres.coh
      
      
     # if (myres$error == FALSE & !all(is.na(myres$est.Sinclair.Z))){
        
        sin3.df <- data.frame(stock = decoder$Short.Name[istock],
                             survey = as.character(surveys[isurvey]),
                             year = rep(myres.sin3$plot.year, 1),
                             Sinclair_Z3 = myres.sin3$est.Sinclair.Z[,1],
                             low90_Sinclair_Z3 = myres.sin3$est.Sinclair.Z[,2],
                             high90_Sinclair_Z3 = myres.sin3$est.Sinclair.Z[,3])
        sin4.df <- data.frame(stock = decoder$Short.Name[istock],
                              survey = as.character(surveys[isurvey]),
                              year = rep(myres.sin4$plot.year, 1),
                              Sinclair_Z4 = myres.sin4$est.Sinclair.Z[,1],
                              low90_Sinclair_Z4 = myres.sin4$est.Sinclair.Z[,2],
                              high90_Sinclair_Z4 = myres.sin4$est.Sinclair.Z[,3])
        sin5.df <- data.frame(stock = decoder$Short.Name[istock],
                              survey = as.character(surveys[isurvey]),
                              year = rep(myres.sin5$plot.year, 1),
                              Sinclair_Z5 = myres.sin5$est.Sinclair.Z[,1],
                              low90_Sinclair_Z5 = myres.sin5$est.Sinclair.Z[,2],
                              high90_Sinclair_Z5 = myres.sin5$est.Sinclair.Z[,3])
        
        sin.df <- merge(sin3.df,sin4.df,by=c("stock","survey","year"),all=TRUE)
        sin.df <- merge(sin.df,sin5.df,by=c("stock","survey","year"),all=TRUE)    
        
        sin.df.dynfa3 <- data.frame(stock = decoder$Short.Name[istock],
                                   survey = as.character(surveys[isurvey]),
                                   Year = rep(myres.sin3.dynfa$plot.year, 1),
                                   year = rep(myres.sin3.dynfa$plot.year, 1),
                                   Sinclair_Z3_dynfa = myres.sin3.dynfa$est.Sinclair.Z[,1],
                                   low90_Sinclair_Z3_dynfa = myres.sin3.dynfa$est.Sinclair.Z[,2],
                                   high90_Sinclair_Z3_dynfa = myres.sin3.dynfa$est.Sinclair.Z[,3])
        sin.df.dynfa4 <- data.frame(stock = decoder$Short.Name[istock],
                                    survey = as.character(surveys[isurvey]),
                                    Year = rep(myres.sin4.dynfa$plot.year-0.5, 1),
                                    year = rep(myres.sin4.dynfa$plot.year, 1),
                                    Sinclair_Z4_dynfa = myres.sin4.dynfa$est.Sinclair.Z[,1],
                                    low90_Sinclair_Z4_dynfa = myres.sin4.dynfa$est.Sinclair.Z[,2],
                                    high90_Sinclair_Z4_dynfa = myres.sin4.dynfa$est.Sinclair.Z[,3])
        sin.df.dynfa5 <- data.frame(stock = decoder$Short.Name[istock],
                                    survey = as.character(surveys[isurvey]),
                                    year = rep(myres.sin5.dynfa$plot.year, 1),
                                    Sinclair_Z5_dynfa = myres.sin5.dynfa$est.Sinclair.Z[,1],
                                    low90_Sinclair_Z5_dynfa = myres.sin5.dynfa$est.Sinclair.Z[,2],
                                    high90_Sinclair_Z5_dynfa = myres.sin5.dynfa$est.Sinclair.Z[,3])
        
        sin.df.dynfa <- merge(sin.df.dynfa3,sin.df.dynfa4,by=c("stock","survey","year"),all=TRUE)
        sin.df.dynfa <- merge(sin.df.dynfa,sin.df.dynfa5,by=c("stock","survey","year"),all=TRUE)
        

        coh.df <- data.frame(stock = decoder$Short.Name[istock],
                             survey = as.character(surveys[isurvey]),
                             year = rep(myres.coh$plot.year,1),
                             cohort_Z = myres.coh$est.cohort.Z[,1],
                             low90_cohort = myres.coh$est.cohort.Z[,2],
                             high90_cohort = myres.coh$est.cohort.Z[,3])
        
        
          
        mdf <- merge(sin.df,sin.df.dynfa,by=c("stock","survey","year"))
        mdf <- merge(x=mdf,y=coh.df,by=c("stock","survey","year"),all=TRUE)
        mdf <- merge(x=mdf,y=coh.ci,by=c("stock","survey","year"),all=TRUE)
        mdf <- merge(x=mdf, y=caa.df,by=c("stock","year"),all=TRUE)
        all.df <- merge(mdf,cs.temp.new,by=c("stock","survey","year"),all=TRUE)
        all.df$region <- decoder$Region[istock]
        
                                                              
        resdf <- rbind(resdf, all.df)
        
      
      #} # enf the survey (j) loop
    }
  }
}


#resdf
#write.csv(resdf, file=".\\figs\\resdf.csv", row.names = FALSE)



year_breaks <- c(1995,2000)
resdf$year_cat <-"1994 and before"
resdf$year_cat[resdf$year>year_breaks[1]] <-"1995 to present"
#resdf$year_cat[resdf$year>=year_breaks[2]] <-"2000 to present"

write.csv(resdf, file=".\\figs\\resdf.csv", row.names = FALSE)




pdf(file = ".//figs//rel_CI_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=relCI_3yr)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Year") +
    ylab("Relative catch / index (3 year avg)") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()

pdf(file = ".//figs//CI_vs_Sinclair_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=relCI_3yr, y=Sinclair_Z, color=year_cat)) +
    geom_point(na.rm = TRUE) +
    scale_color_manual(values = c("red", "blue") )+
    
    #geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Relative catch / index (3 year avg)") +
    ylab("Sinclair Z (3 yr)") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()


pdf(file = ".//figs//CI_vs_Catch_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=relCI_3yr, y=CAA_Z, color=year_cat)) +
    geom_point(na.rm = TRUE) +
    scale_color_manual(values = c("red", "blue") )+
    #geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Relative catch / index (3 year avg)") +
    ylab("Sinclair Z (3 yr)") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()


# Sinclair Z plots
pdf(file = ".//figs//Sinclair_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=Sinclair_Z)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    geom_ribbon(aes(ymin=low90_Sinclair, ymax=high90_Sinclair), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Year") +
    ylab("Sinclair Z") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()

## formula method, "graph" layout (row 1 at bottom):
quartz()
pairs(~ Sinclair_Z + Sinclair_Z_dynfa + CAA_Z, data = resdf, row1attop=FALSE,
      #subset = Education < 20, 
      main = "Z comparisons")




pdf(file = ".//figs//Cohort_CI_vs_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=cohort_CI, y=cohort_Z, color=year_cat)) +
    geom_point(na.rm = TRUE) +
    scale_color_manual(values = c("red", "blue") )+
    #geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Cohort catch / index") +
    ylab("Cohort Z ") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()


# Figure 1
# 
gb.ind <- 8
thisstock <- decoder$Short.Name[gb.ind]
leg.size = 14
p1.1 <- ggplot(gbyt.sa.dat, aes(x=Year,y=SSB,color=Source))+
            geom_line(na.rm = TRUE, size = 1.5) +
            xlab("Year") +
            ylab("SSB")+
            theme_bw()+
            theme_classic(base_family = "Times New Roman",base_size = leg.size)+
            theme(legend.position=c(0.2,0.8))+
            scale_color_manual(values = c("red","gray","blue","green","black") )
  #scale_shape_manual(values=c(16, 17, 15,3))+
  #coord_cartesian(xlim=c(0.65,1.0),ylim=c(0,1.)) +
  
print(p1.1)


  
p1.3 <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=Sinclair_Z_dynfa,color=survey)) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  scale_color_manual(values = c("blue","red"))+
  #geom_ribbon(aes(ymin=low90_Sinclair, ymax=high90_Sinclair), alpha=0.3) +
  #facet_wrap(~survey) +
  xlab("Year") +
  ylab("Sinclair Z") +
  #ggtitle(thisstock) +
  theme_bw()+
  theme_classic(base_family = "Times New Roman",base_size = leg.size)+
  theme(legend.position=c(0.2,0.9))
  print(p1.3)


# # Cohort Z plots
# pdf(file = ".//figs//cohort_Z_plots.pdf")
# use.stocks = nstocks
# for (istock in 1:use.stocks){
#   thisstock <- decoder$Short.Name[istock]
#   p <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=cohort_Z)) +
#     geom_point(na.rm = TRUE) +
#     geom_line(na.rm = TRUE) +
#     #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
#     facet_wrap(~survey) +
#     xlab("Year of cohort birth") +
#     ylab("Cohort Z") +
#     ggtitle(thisstock) +
#     theme_bw()
#   print(p)
# }
# dev.off()
# 
# pdf(file = ".//figs//CAA_Z_plots.pdf")
# use.stocks = nstocks
# for (istock in 1:use.stocks){
#   thisstock <- decoder$Short.Name[istock]
#   p <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=CAA_Z)) +
#     geom_point(na.rm = TRUE) +
#     geom_line(na.rm = TRUE) +
#     geom_ribbon(aes(ymin=low90_CAA, ymax=high90_CAA), alpha=0.3) +
#     facet_wrap(~survey) +
#     xlab("Year") +
#     ylab("Fishery catch-at-age Z") +
#     ggtitle(thisstock) +
#     theme_bw()
#   print(p)
# }
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ci.yr <- seq(1980,2011,1)
# ci.age <- seq(1,11,1)
# quartz()
# for(i in 1:length(ci.yr))
# {
#   cid <- ci.df[ci.df$cohort==ci.yr[i],]
#   if(i==1)plot(cid$age,cid$CI/max(cid$CI,na.rm=TRUE),type='l',lty=i,col=i,ylim=c(0,1))
#   else lines(cid$age,cid$CI/max(cid$CI,na.rm=TRUE),col=i,lty=i)
# }
