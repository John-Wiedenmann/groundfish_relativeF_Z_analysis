
library("ASAPplots")
library("dplyr")
library("ggplot2")
library(gridExtra)
library(grid)

# to do: 
# add GOM haddock data
# calculate CCZ using the DLMTool method
# calculate Sinclair Z using the fishery CAA
# PLot relative CI for stocks by region and year, calculate and overall average for each region
# Plot cohort CI and Z to see some of the extremes

# make sure herring is using more years
# fix multpanle figure for mackerel

{
user <- "MAC"  # Use ("MAC" or "PC") - there are different ways to access directories \ or / 

if(user=="MAC") 
{
  source(".//Rcode//Groundfish_functions.R")
  decoder <- read.csv(".//ADIOS_data//file_decoder.csv")
  so_use <- read.csv(".//ADIOS_data//survey_options_use.csv", stringsAsFactors = FALSE)
  #so_use <- so_use[so_use$survey=="NMFS fall BTS" | so_use$survey=="NMFS spring BTS",]
  
  caa.dat <- read.table(".//ADIOS_data//NEFSC_CAA.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  ctot.dat <-read.table(".//ADIOS_data//NEFSC_total_catch.txt",sep="\t",header=TRUE,stringsAsFactors = FALSE)
  stot.dat <-read.table(".//ADIOS_data//NEFSC_annual_survey.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  gbyt.sa.dat <- read.table(".//ADIOS_data//GB_yellowtail_SA_estimates.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  gbyt.retro.dat <- read.csv(".//ADIOS_data//GBYT_SSB_Retro_mod1.csv",header=TRUE, stringsAsFactors = FALSE)
  rho <- read.table(".//ADIOS_data//Rho_estimates.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
 
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
  gbyt.retro.dat <- read.table(".\\ADIOS_data\\GBYT_SSB_retro.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  rho <- read.table(".\\ADIOS_data\\Rho_estimates.txt",sep="\t",header=TRUE, stringsAsFactors = FALSE)
  
}  
  
  cs.dat <- merge(x=stot.dat,y=ctot.dat,by=c("stock","year"),all=TRUE)
  cs.dat$CIW <- cs.dat$catch/cs.dat$kg_tow  # calculate catch per index using kg / tow
  cs.dat$CIN <- cs.dat$catch/cs.dat$n_tow  # calculate catch per index using kg / tow
  
  
}



nstocks <- length(decoder$Short.Name)
res <- list()
resdf <- data.frame()
#for (istock in 11:11){
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
                                     low90_CAA_Z = NA,
                                     high90_CAA_Z = NA,
                                     year.avg = 3)
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
                       year = rep(myres.sin.caa3$plot.year,1),
                       year.avg = 3,
                       CAA_Z = myres.sin.caa3$est.Sinclair.Z[,1],
                       low90_CAA_Z = myres.sin.caa3$est.Sinclair.Z[,2],
                       high90_CAA_Z = myres.sin.caa3$est.Sinclair.Z[,3])
  
  caa.df4 <- data.frame(stock = decoder$Short.Name[istock],
                        year = rep(myres.sin.caa4$plot.year,1),
                        year.avg = 4,
                        CAA_Z = myres.sin.caa4$est.Sinclair.Z[,1],
                        low90_CAA_Z = myres.sin.caa4$est.Sinclair.Z[,2],
                        high90_CAA_Z = myres.sin.caa4$est.Sinclair.Z[,3])
  
  caa.df5 <- data.frame(stock = decoder$Short.Name[istock],
                        year = rep(myres.sin.caa5$plot.year,1),
                        year.avg = 5,
                        CAA_Z = myres.sin.caa5$est.Sinclair.Z[,1],
                        low90_CAA_Z = myres.sin.caa5$est.Sinclair.Z[,2],
                        high90_CAA_Z = myres.sin.caa5$est.Sinclair.Z[,3])
  
  caa.df <- rbind(caa.df3,caa.df4,caa.df5)

  }
  # loop over the surveys
  for (isurvey in 1:nsurveys)
  {
    # this is for the CI calculations
    cs.temp <- filter(cs.dat, stock == decoder$Short.Name[istock], 
                      survey == surveys[isurvey], year >=1978 & year<=2014)
  
    relCIW_min_yr <- 1985
    
    # this is account for period without Z estimates but really high 
    # C/I which makes all the points for herring at C/I < 0.25
     if(decoder$Short.Name[istock]=='herring')
     {
       cs.temp <- filter(cs.dat, stock == decoder$Short.Name[istock],
                         survey == surveys[isurvey], year >=1984)
       relCIW_min_yr <- 1988
       
      }


    # below accounts for empty catch / survey dataframes for some of the stocks
    cs.dim <-dim(cs.temp)
    if(cs.dim[1]==0){
      cs.temp <- data.frame(matrix(NA,nrow=1,ncol=cs.dim[2]))
      colnames(cs.temp) <- colnames(cs.dat)
      cs.temp$stock <- decoder$Short.Name[istock]
      cs.temp$survey <- surveys[isurvey]
      cs.temp$year <- 2000
      cs.temp$relCIN <- NA
      cs.temp$relCIW <- NA
      cs.temp$CIN_avg <- NA
      cs.temp$CIW_avg <- NA
      cs.temp$relCIN_avg <-NA
      cs.temp$relCIW_avg <-NA
      cs.temp$year.avg <- 3
      
      cs.temp.new <- cs.temp
    }
    
    else
    {
      
      # 3 year window
      cs.temp3 <- cs.temp
      cs.temp3$relCIN <- cs.temp$CIN/mean(cs.temp$CIN[cs.temp$year>=relCIW_min_yr],na.rm=TRUE)
      cs.temp3$relCIW <- cs.temp$CIW/mean(cs.temp$CIW[cs.temp$year>=relCIW_min_yr],na.rm=TRUE)
      Avg_CIN <- move.avg.2(cbind(cs.temp$year,cs.temp$CIN),3)
      cs.temp3$CIN_avg <- as.numeric(Avg_CIN[,2])
      Avg_CIW <- move.avg.2(cbind(cs.temp$year,cs.temp$CIW),3)
      cs.temp3$CIW_avg <- as.numeric(Avg_CIW[,2])
      cs.temp3$relCIN_avg <- cs.temp3$CIN_avg/mean(cs.temp3$CIN_avg[cs.temp3$year>=relCIW_min_yr],na.rm=TRUE)  # 3 year moving average
      cs.temp3$relCIW_avg <- cs.temp3$CIW_avg/mean(cs.temp3$CIW_avg[cs.temp3$year>=relCIW_min_yr],na.rm=TRUE)  # 3 year moving average
      cs.temp3$year.avg <- 3
      
      # 4 year window
      cs.temp4 <- cs.temp
      cs.temp4$year = cs.temp4$year+0.5
      cs.temp4$relCIN <- cs.temp$CIN/mean(cs.temp$CIN[cs.temp$year>=relCIW_min_yr],na.rm=TRUE)
      cs.temp4$relCIW <- cs.temp$CIW/mean(cs.temp$CIW[cs.temp$year>=relCIW_min_yr],na.rm=TRUE)
      Avg_CIN <- move.avg.2(cbind(cs.temp$year,cs.temp$CIN),4)
      cs.temp4$CIN_avg <- as.numeric(Avg_CIN[,2])
      Avg_CIW <- move.avg.2(cbind(cs.temp$year,cs.temp$CIW),4)
      cs.temp4$CIW_avg <- as.numeric(Avg_CIW[,2])
      cs.temp4$relCIN_avg <- cs.temp4$CIN_avg/mean(cs.temp4$CIN_avg[cs.temp4$year>=relCIW_min_yr],na.rm=TRUE)  # 4 year moving average
      cs.temp4$relCIW_avg <- cs.temp4$CIW_avg/mean(cs.temp4$CIW_avg[cs.temp4$year>=relCIW_min_yr],na.rm=TRUE)  # 4 year moving average
      cs.temp4$year.avg <- 4
     
      # 5 year window
      cs.temp5 <- cs.temp
      cs.temp5$relCIN <- cs.temp$CIN/mean(cs.temp$CIN[cs.temp$year>=relCIW_min_yr],na.rm=TRUE)
      cs.temp5$relCIW <- cs.temp$CIW/mean(cs.temp$CIW[cs.temp$year>=relCIW_min_yr],na.rm=TRUE)
      Avg_CIN <- move.avg.2(cbind(cs.temp$year,cs.temp$CIN),5)
      cs.temp5$CIN_avg <- as.numeric(Avg_CIN[,2])
      Avg_CIW <- move.avg.2(cbind(cs.temp$year,cs.temp$CIW),5)
      cs.temp5$CIW_avg <- as.numeric(Avg_CIW[,2])
      cs.temp5$relCIN_avg <- cs.temp5$CIN_avg/mean(cs.temp5$CIN_avg[cs.temp5$year>=relCIW_min_yr],na.rm=TRUE)  
      cs.temp5$relCIW_avg <- cs.temp5$CIW_avg/mean(cs.temp5$CIW_avg[cs.temp$year>=relCIW_min_yr],na.rm=TRUE)
      cs.temp5$year.avg <- 5
      
      # combine the different estimates into single DF
      cs.temp.new <- rbind(cs.temp3,cs.temp4,cs.temp5)
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
      
      #myres.coh <- calc_cohort_Z(mat,8)
      myres.sin3.dynfa <- calc_Sinclair_Z_dynfa(all.mat,3)
      myres.sin4.dynfa <- calc_Sinclair_Z_dynfa(all.mat,4)
      myres.sin5.dynfa <- calc_Sinclair_Z_dynfa(all.mat,5)
      
      # if(dim(caa)[1]==0)
      # {
      #   coh.ci <-data.frame(year=2000,cohort_CI=NA,
      #                       stock= decoder$Short.Name[istock],
      #                       survey=as.character(surveys[isurvey]),rel_cohCI=NA)
      # }
      # else
      # {
      #   #coh.ci <- calc_cohort_CI(all.mat,caa.mat,8)
      #   if(cur.stock!="herring")
      #   {
      #     coh.ci <- calc_cohort_CI(mat,caa.mat,8)
      #     coh.ci$stock <- decoder$Short.Name[istock]
      #     coh.ci$survey <-as.character(surveys[isurvey])
      #     coh.ci$rel_cohCI <- coh.ci$cohort_CI/mean(coh.ci$cohort_CI,na.rm=TRUE)
      #   }
      #   
      #   else
      #   {
      #     coh.ci <- NA
      #     coh.ci$stock <- NA
      #     coh.ci$survey <-as.character(surveys[isurvey])
      #     coh.ci$rel_cohCI <- NA
      #   }
      # 
      # }
      
      res[[istock]]$z.sin[[isurvey]] <- myres.sin3
      #res[[istock]]$z.coh[[isurvey]] <- myres.coh
      
      
     # if (myres$error == FALSE & !all(is.na(myres$est.Sinclair.Z))){
        
        sin3.df <- data.frame(stock = decoder$Short.Name[istock],
                             survey = as.character(surveys[isurvey]),
                             year = rep(myres.sin3$plot.year, 1),
                             year.avg = 3,
                             Sinclair_Z = myres.sin3$est.Sinclair.Z[,1],
                             low90_Sinclair_Z = myres.sin3$est.Sinclair.Z[,2],
                             high90_Sinclair_Z = myres.sin3$est.Sinclair.Z[,3])
        
        sin4.df <- data.frame(stock = decoder$Short.Name[istock],
                              survey = as.character(surveys[isurvey]),
                              year = rep(myres.sin4$plot.year, 1),
                              year.avg = 4,
                              Sinclair_Z = myres.sin4$est.Sinclair.Z[,1],
                              low90_Sinclair_Z = myres.sin4$est.Sinclair.Z[,2],
                              high90_Sinclair_Z = myres.sin4$est.Sinclair.Z[,3])
        
        sin5.df <- data.frame(stock = decoder$Short.Name[istock],
                              survey = as.character(surveys[isurvey]),
                              year = rep(myres.sin5$plot.year, 1),
                              year.avg = 5,
                              Sinclair_Z = myres.sin5$est.Sinclair.Z[,1],
                              low90_Sinclair_Z = myres.sin5$est.Sinclair.Z[,2],
                              high90_Sinclair_Z = myres.sin5$est.Sinclair.Z[,3])
        
        sin.df <- rbind(sin3.df,sin4.df,sin5.df)
        #sin.df <- merge(sin3.df,sin4.df,by=c("stock","survey","year"),all=TRUE)
        #sin.df <- merge(sin.df,sin5.df,by=c("stock","survey","year"),all=TRUE)    
        
        sin.df.dynfa3 <- data.frame(stock = decoder$Short.Name[istock],
                                   survey = as.character(surveys[isurvey]),
                                   year = rep(myres.sin3.dynfa$plot.year, 1),
                                   year.avg = 3,
                                   Sinclair_Z_dynfa = myres.sin3.dynfa$est.Sinclair.Z[,1],
                                   low90_Sinclair_Z_dynfa = myres.sin3.dynfa$est.Sinclair.Z[,2],
                                   high90_Sinclair_Z_dynfa = myres.sin3.dynfa$est.Sinclair.Z[,3])
        sin.df.dynfa4 <- data.frame(stock = decoder$Short.Name[istock],
                                    survey = as.character(surveys[isurvey]),
                                    year = rep(myres.sin4.dynfa$plot.year, 1),
                                    year.avg = 4,
                                    Sinclair_Z_dynfa = myres.sin4.dynfa$est.Sinclair.Z[,1],
                                    low90_Sinclair_Z_dynfa = myres.sin4.dynfa$est.Sinclair.Z[,2],
                                    high90_Sinclair_Z_dynfa = myres.sin4.dynfa$est.Sinclair.Z[,3])
        sin.df.dynfa5 <- data.frame(stock = decoder$Short.Name[istock],
                                    survey = as.character(surveys[isurvey]),
                                    year = rep(myres.sin5.dynfa$plot.year, 1),
                                    year.avg = 5,
                                    Sinclair_Z_dynfa = myres.sin5.dynfa$est.Sinclair.Z[,1],
                                    low90_Sinclair_Z_dynfa = myres.sin5.dynfa$est.Sinclair.Z[,2],
                                    high90_Sinclair_Z_dynfa = myres.sin5.dynfa$est.Sinclair.Z[,3])
       
         sin.df.dynfa <- rbind(sin.df.dynfa3,sin.df.dynfa4,sin.df.dynfa5)
                
       # sin.df.dynfa <- merge(sin.df.dynfa3,sin.df.dynfa4,by=c("stock","survey","year"),all=TRUE)
      #  sin.df.dynfa <- merge(sin.df.dynfa,sin.df.dynfa5,by=c("stock","survey","year"),all=TRUE)
        

        # coh.df <- data.frame(stock = decoder$Short.Name[istock],
        #                      survey = as.character(surveys[isurvey]),
        #                      year = rep(myres.coh$plot.year,1),
        #                      cohort_Z = myres.coh$est.cohort.Z[,1],
        #                      low90_cohort = myres.coh$est.cohort.Z[,2],
        #                      high90_cohort = myres.coh$est.cohort.Z[,3])
        
        
          
        mdf <- merge(sin.df,sin.df.dynfa,by=c("stock","survey","year","year.avg"))
        mdf <- merge(x=mdf, y=caa.df,by=c("stock","year","year.avg"),all=TRUE)
        #mdf <- merge(x=mdf,y=coh.df,by=c("stock","survey","year"),all=TRUE)
       # mdf <- merge(x=mdf,y=coh.ci,by=c("stock","survey","year"),all=TRUE)
        all.df <- merge(mdf,cs.temp.new,by=c("stock","survey","year","year.avg"),all=TRUE)
        all.df$region <- decoder$Region[istock]
        all.df$species <- decoder$Species[istock]
        all.df$rel_Zsin <- all.df$Sinclair_Z/(mean(all.df$Sinclair_Z,na.rm=TRUE))                                                      
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
resdf<- resdf[!is.na(resdf$year.avg),]
resdf<-resdf[!is.na(resdf$survey),]
resdf$year.mod<-0  # this is to create an indicator if its an integer year or in between
resdf$year.mod[resdf$year%%1>0]=1 # used for subsetting when year.avg==4

# ifelse(user=="MAC",write.csv(resdf, file=".//figs//resdf.csv", row.names = FALSE),
#        write.csv(resdf, file=".\\figs\\resdf.csv", row.names = FALSE))



tdat <- filter(resdf,year.avg==4)
Avg_by_survey <- as.data.frame.table(tapply(tdat$relCIN_avg,list(tdat$region,
                                      tdat$year,tdat$survey),mean,na.rm=TRUE))
colnames(Avg_by_survey) <-c("region","year","survey","relCIN_savg")
Avg_by_survey$year =  as.numeric(levels(Avg_by_survey$year))[Avg_by_survey$year]
Avg_by_survey$region =  as.factor(levels(Avg_by_survey$region))[Avg_by_survey$region]
Avg_by_survey$survey =  as.factor(levels(Avg_by_survey$survey))[Avg_by_survey$survey]
Avg_by_survey$relCIW_savg<-as.vector(tapply(tdat$relCIW_avg,list(tdat$region,
                                        tdat$year,tdat$survey),mean,na.rm=TRUE))
Avg_by_survey$Sinclair_Z_savg<-as.vector(tapply(tdat$Sinclair_Z,list(tdat$region,
                                        tdat$year,tdat$survey),mean,na.rm=TRUE))
Avg_by_survey$stock <- as.factor("Average")
Avg_by_survey$year.avg <-4


tdat <- filter(resdf,year.avg==4,year.mod==1)
t3dat <- filter(resdf,year.avg==3,year.mod==0)
Avg_region<- as.data.frame.table(tapply(tdat$relCIN,list(tdat$region,
                                       tdat$year),mean,na.rm=TRUE))
colnames(Avg_region) <-c("region","year","relCIN_region")
Avg_region$year =  as.numeric(levels(Avg_region$year))[Avg_region$year]
Avg_region$region =  as.factor(levels(Avg_region$region))[Avg_region$region]
Avg_region$relCIW_region<-as.vector(tapply(tdat$relCIW,list(tdat$region,
                                                  tdat$year),mean,na.rm=TRUE))
Avg_region$Z_avg<-as.vector(tapply(tdat$Sinclair_Z,list(tdat$region,
                                                  tdat$year),mean,na.rm=TRUE))
Avg_region$relZ_avg<-as.vector(tapply(tdat$rel_Zsin,list(tdat$region,
                                                        tdat$year),mean,na.rm=TRUE))
#Avg_region$rel_cohCI_region <- as.vector(tapply(t3dat$rel_cohCI,list(t3dat$region,
#                                                        t3dat$year),mean,na.rm=TRUE))
Avg_region$stock <-as.factor("Average")
Avg_region$species <-as.factor("Average")



td94 <- filter(resdf,year.avg==4,year.mod==1)
Avg_94<- as.data.frame.table(tapply(td94$relCIW_avg,list(td94$stock,td94$year_cat),mean,na.rm=TRUE))
colnames(Avg_94) <-c("stock","year_cat","relCIW_pd")
Avg_94$stock =  as.factor(levels(Avg_94$stock))[Avg_94$stock]
Avg_94$year_cat =  as.factor(levels(Avg_94$year_cat))[Avg_94$year_cat]
Avg_94$relCIN_pd<-as.vector(tapply(td94$relCIN_avg,list(td94$stock,td94$year_cat),mean,na.rm=TRUE))
Avg_94$Zs_pd<-as.vector(tapply(td94$Sinclair_Z,list(td94$stock,td94$year_cat),mean,na.rm=TRUE))
Avg_94$Zf_pd<-as.vector(tapply(td94$CAA_Z,list(td94$stock,td94$year_cat),mean,na.rm=TRUE))



df_avg <- merge(resdf,Avg_region,by=c("year","region","stock","species"),all=TRUE)
df_avg$plot_region = "C) Southern New England / Mid-Atlantic"
df_avg$plot_region[df_avg$region=="GOM"] = "A) Gulf of Maine"
df_avg$plot_region[df_avg$region=="GB"] = "B) Georges Bank"
df_avg$plot_region[df_avg$region=="Pelagic"] = "D) Pelagic"




################# Figure 1 GB yellowtail #######
{
gbyt.sa.dat$Source <-factor(gbyt.sa.dat$Source,levels=c("NEFSC 2002","NEFSC 2005","NEFSC 2008",
                                                        "Legault et al. 2011", "Legault et al. 2013"))
leg.size = 12


p1.1 <- ggplot(filter(gbyt.retro.dat,Year >=1978), aes(x=Year,y=SSB/10000,color=as.factor(Peel)))+
  geom_line(data=filter(gbyt.retro.dat,Peel ==7), size = 1.5) +
  geom_line(data=filter(gbyt.retro.dat,Peel ==6), size = 1.5) +
  geom_line(data=filter(gbyt.retro.dat,Peel ==5), size = 1.5) +
  geom_line(data=filter(gbyt.retro.dat,Peel ==4), size = 1.5) +
  geom_line(data=filter(gbyt.retro.dat,Peel ==3), size = 1.5) +
  geom_line(data=filter(gbyt.retro.dat,Peel ==2), size = 1.5) +
  geom_line(data=filter(gbyt.retro.dat,Peel ==1), size = 1.5) +
  geom_line(data=filter(gbyt.retro.dat,Peel ==0), size = 1.5) +
  xlab(" ") +
  ylab("SSB (x 10,000 mt)")+
  coord_cartesian(xlim=c(1978,2014),ylim=c(0,4))+
  ggtitle("A")+
  scale_x_continuous(limits = c(1978, 2014))+
  theme_bw()+
  theme_classic(base_family = "Times",base_size = leg.size)+
  #theme(legend.position=c(0.4,0.7),legend.text=element_text(size=9),legend.title=element_blank())+
  theme(legend.position=c(0.4,0.7),legend.text=element_text(size=9))+
  guides(color=guide_legend(title="Years removed",ncol=2))+
  scale_color_manual(values = c("black","gray","purple","blue","green","yellow","orange","red") )
print(p1.1)

p1.2 <- ggplot(filter(gbyt.sa.dat,Source!="Legault et al. 2011"), aes(x=Year,y=SSB/10000,color=Source))+
  geom_line(na.rm = FALSE, size = 1.5) +
  xlab(" ") +
  ylab("SSB (x 10,000 mt)")+
  coord_cartesian(xlim=c(1978,2014))+
  ggtitle("B")+
  scale_x_continuous(limits = c(1978, 2014))+
  theme_bw()+
  theme_classic(base_family = "Times",base_size = leg.size)+
  #theme(legend.position=c(0.4,0.7),legend.text=element_text(size=9),legend.title=element_blank())+
  theme(legend.position=c(0.4,0.7),legend.text=element_text(size=9))+
  guides(color=guide_legend(title="Assessment"))+
  scale_color_manual(values = c("red","gray","blue","black","green") )
print(p1.2)

p1.3 <- ggplot(filter(resdf, stock == "GByt",year.avg==4,year.mod==1), aes(x=year, 
                                                               y=relCIW,color=survey,fill=survey)) +
  #geom_ribbon(aes(ymin=low90_Sinclair_Z, ymax=high90_Sinclair_Z,fill=factor(survey)),alpha=0.3,colour=NA) +
  geom_line(na.rm = TRUE,size=1.5,aes(linetype=survey)) +
  scale_color_manual(values = c("red","blue"))+
  xlab("") +
  ylab("Relative F") +
  ggtitle("C")+
  coord_cartesian(xlim=c(1978,2014))+
  scale_x_continuous(limits = c(1978, 2014))+
  theme_bw()+
  theme_classic(base_family = "Times",base_size = leg.size)+
  theme(legend.position=c(0.8,0.9),legend.text=element_text(size=9),legend.title=element_blank())
print(p1.2)

p1.4 <- ggplot(filter(resdf, stock == "GByt",year.avg==4,year.mod==1), aes(x=year, 
                                        y=Sinclair_Z,color=survey,fill=survey)) +
  geom_ribbon(aes(ymin=low90_Sinclair_Z, ymax=high90_Sinclair_Z,fill=factor(survey)),alpha=0.3,colour=NA) +
  geom_line(na.rm = TRUE,size=1.5,aes(linetype=survey)) +
  scale_color_manual(values = c("red","blue"))+
  xlab("") +
  ylab("Total mortality (Z)") +
  ggtitle("D")+
  coord_cartesian(xlim=c(1978,2014))+
  scale_x_continuous(limits = c(1978, 2014))+
  theme_bw()+
  theme_classic(base_family = "Times",base_size = leg.size)+
  theme(legend.position=c(0.55,0.95),legend.text=element_text(size=9),legend.title=element_blank())
print(p1.3)

quartz()
p1.all <- grid.arrange(p1.1,p1.2,p1.3,p1.4,nrow=2)
}

########## Figure 2 annual C/I  by Region ######
df_avg$plot_region = "C) Southern New England / Mid-Atlantic"
df_avg$plot_region[df_avg$region=="GOM"] = "A) Gulf of Maine"
df_avg$plot_region[df_avg$region=="GB"] = "B) Georges Bank"
df_avg$plot_region[df_avg$region=="Pelagic"] = "D) Pelagic"

df_avg$relCIW_region[df_avg$stock=="Average" & df_avg$region=="Pelagic" & df_avg$year <1983]=NA

pdf(file = ".//figs//Avg_CI_by_region.pdf")
{
  
  p2.CI.all<- ggplot(filter(df_avg, year.avg==4,region!="Pelagic"), aes(x=year, y=relCIW, color=species)) +
    geom_line(na.rm = TRUE,aes(linetype=survey)) +
    geom_line(data=filter(df_avg,stock=="Average",region!="Pelagic"),
              color="black",size=1.25,aes(x=year,y=relCIW_region))+
    coord_cartesian(xlim=c(1982,2014),ylim=c(0,5))+
    scale_x_continuous(limits = c(1982, 2014))+
    
    facet_wrap(c("plot_region"),nrow=3) +
    xlab("Year") +
    ylab("Relative F") +
    theme_bw()+
    #theme_classic(base_family = "Times New Roman",base_size = leg.size)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          text=element_text(size=12,  family="Times"),strip.text = element_text(size = 12, hjust = 0))
  quartz()
  print(p2.CI.all)
}
dev.off()

pdf(file = ".//figs//Avg_CI_by_region_with_pelagcis.pdf")
{
  
  p2.CI.all<- ggplot(filter(df_avg, year.avg==4), aes(x=year, y=relCIW, color=species)) +
    geom_line(na.rm = TRUE,aes(linetype=survey)) +
    geom_line(data=filter(df_avg,stock=="Average"),
              color="black",size=1.25,aes(x=year,y=relCIW_region))+
    coord_cartesian(xlim=c(1982,2014),ylim=c(0,5))+
    scale_x_continuous(limits = c(1982, 2014))+
    
    facet_wrap(c("plot_region"),nrow=2) +
    xlab("Year") +
    ylab("Relative F") +
    theme_bw()+
    #theme_classic(base_family = "Times New Roman",base_size = leg.size)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          text=element_text(size=12,  family="Times"),strip.text = element_text(size = 12, hjust = 0))
  quartz()
  print(p2.CI.all)
}
dev.off()


pdf(file = ".//figs//Avg_Z_by_region.pdf")
{
  
  p3.CI.all<- ggplot(filter(df_avg, year.avg==4,region!="Pelagic"), aes(x=year, y=rel_Zsin, color=species)) +
    geom_line(na.rm = TRUE,aes(linetype=survey)) +
    geom_line(data=filter(df_avg,stock=="Average",region!="Pelagic"),
              color="black",size=1.25,aes(x=year,y=relZ_avg))+
    coord_cartesian(xlim=c(1982,2014),ylim=c(0,3))+
    scale_x_continuous(limits = c(1982, 2014))+
    
    facet_wrap(c("plot_region"),nrow=3) +
    xlab("Year") +
    ylab("Z (relative to the mean)") +
    theme_bw()+
    #theme_classic(base_family = "Times New Roman",base_size = leg.size)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          text=element_text(size=12,  family="Times"),strip.text = element_text(size = 12, hjust = 0))
  quartz()
  print(p3.CI.all)
}
dev.off()

# Avg Z by region with pelagics
{
  p3.CI.all<- ggplot(filter(df_avg, year.avg==4), aes(x=year, y=rel_Zsin, color=species)) +
    geom_line(na.rm = TRUE,aes(linetype=survey)) +
    geom_line(data=filter(df_avg,stock=="Average"),
              color="black",size=1.25,aes(x=year,y=relZ_avg))+
    coord_cartesian(xlim=c(1982,2014),ylim=c(0,3))+
    scale_x_continuous(limits = c(1982, 2014))+
    
    facet_wrap(c("plot_region"),nrow=2) +
    xlab("Year") +
    ylab("Z (relative to the mean)") +
    theme_bw()+
    #theme_classic(base_family = "Times New Roman",base_size = leg.size)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          text=element_text(size=12,  family="Times"),strip.text = element_text(size = 12, hjust = 0))
  quartz()
  print(p3.CI.all)
}



############### Figure 4 C/I vs. Z by stock   ##########

stocks <- c("GBcod","GOMcod","GBhaddock","GOMhaddock",
            "GByt","CCGOMyt","SNEMAyt","GBwinter","SNEMAwinter","witch","plaice",
            "pollock","whitehake","redfish","fluke","scup","mackerel","herring")
stock.names <-c("A) GB cod","B) GOM cod","C) GB haddock","D) GOM haddock",
               "E) GB yellowtail flounder","F) CC/GOM yellowtail flounder","G) SNE/MA yellowtail flounder",
               "H) GB winter flounder","I) SNE/MA winter flounder",
               "J) witch flounder","K) plaice","L) pollock","M) whitehake","N) redfish", "O) summer flounder","P) scup","Q) mackerel","R) herring")

stock.names.spring1 <-c("A) GB cod (spring)","C) GOM cod (spring)","E) GB haddock (spring)","G) GOM haddock (spring)",
                "I) GB yellowtail flounder (spring)","K) CC/GOM yellowtail flounder (spring)","M) SNE/MA yellowtail flounder (spring)" ,
                "O) GB winter flounder (spring)","Q) SNE/MA winter flounder (spring)")

stock.names.spring2 <-c("A) witch flounder (spring)","C) plaice (spring)","E) pollock (spring)","G) whitehake (spring)","I) redfish (spring)", "J) summer flounder (spring)",
                        "L) scup (spring)","N) mackerel (spring)","O) herring (spring)")

stock.names.fall1 <-c("B) GB cod (fall)","D) GOM cod (fall)","F) GB haddock (fall)","H) GOM haddock (fall)",
                        "J) GB yellowtail flounder (fall)","L) CC/GOM yellowtail flounder (fall)","N) SNE/MA yellowtail flounder (fall)" ,
                        "P) GB winter flounder (fall)","R) SNE/MA winter flounder (fall)")                     
                     
stock.names.fall2 <-c("B) witch flounder (fall)","D) plaice (fall)","F) pollock (fall)","H) whitehake (fall)","NULL", "J) summer flounder (fall)",
                        "M) scup (fall)","NULL","P) herring (fall)")

stock.names.fall <- c(stock.names.fall1,stock.names.fall2)
stock.names.spring <- c(stock.names.spring1,stock.names.spring2)

plots <- list()  # sinclair Z survey plots
sp_plots <- list()  # sinclair Z survey plots
f_plots <- list()  # sinclair Z survey plots

cz.plots <- list()
coh.plots <- list()
slope_CIZ <- cor_CIZs <-slope_cohCIZ   <- cor_CIZf <-length(stocks) <- cor_relCI <- length(stocks)
cor_survZ <- cor_fall_caa_Z <- cor_spring_caa_Z <- cor_coh_CIZ<-length(stocks)
cor_CIZs_spring <- cor_CIZs_fall <- length(stocks)
Zs_pre <- Zs_post <- Zf_pre <- Zf_post <- relCI_pre <- relCI_post <- length(stocks)
Zrat <- CIrat <- length(stocks)
xlims<-c(0,3)
coh.xlims <-c(0,5)
pre <- "1994 and before"
post <- "1995 to present"
for(s in 1:length(stocks))
{
  stock.name = stocks[s]
  ci.var <- "relCIW_avg"
  z.var <- "Sinclair_Z"
  cz.var <- "CAA_Z"
  
  xlims<-c(0,3)
  #if( stock.name=="herring") xlims<-c(0,1)
  
  Zs_pre[s] <- Avg_94$Zs_pd[Avg_94$stock==stock.name & Avg_94$year_cat==pre]
  Zs_post[s] <- Avg_94$Zs_pd[Avg_94$stock==stock.name & Avg_94$year_cat==post]
  Zf_pre[s] <- Avg_94$Zf_pd[Avg_94$stock==stock.name & Avg_94$year_cat==pre]
  Zf_post[s] <- Avg_94$Zf_pd[Avg_94$stock==stock.name & Avg_94$year_cat==post]
  relCI_pre[s] <- Avg_94$relCIW_pd[Avg_94$stock==stock.name & Avg_94$year_cat==pre]
  relCI_post[s] <- Avg_94$relCIW_pd[Avg_94$stock==stock.name & Avg_94$year_cat==post]
  
  Zrat[s] <- Zs_post[s] / Zs_pre[s]
  CIrat[s] <- relCI_post[s] / relCI_pre[s]
  
  resdf$plot.name[resdf$stock==stocks[s]] <- stock.names[s]
    
  pdat <- filter(resdf,year.avg==4,year.mod==1,stock==stock.name)
  attach(pdat)
  
  ylims<-c(0,2)
  if(stock.name=="mackerel") ylims<-c(0,2)
  
  lm1 <- lm(pdat$Sinclair_Z~pdat$relCIW_avg)
  slope_CIZ[s] <-lm1$coefficients[2]
  cor_CIZs[s] <- cor(pdat$relCIW_avg,pdat$Sinclair_Z,use="na.or.complete")
  
  cor_CIZf[s] <- cor(pdat$relCIW_avg,pdat$CAA_Z,use="na.or.complete")
  fdat <- pdat[pdat$survey=="NMFS fall BTS",c("year","survey","year_cat",ci.var,z.var,cz.var)]
  spdat <- pdat[pdat$survey=="NMFS spring BTS",c("year","survey","year_cat",ci.var,z.var,cz.var)]
  
  cor_CIZs_spring[s] <- cor(spdat$relCIW_avg,spdat$Sinclair_Z,use="na.or.complete")
  cor_CIZs_fall[s] <- cor(fdat$relCIW_avg,fdat$Sinclair_Z,use="na.or.complete")
  

  ciz.dat <-merge(fdat,spdat,by=c("year"),all=TRUE)  
  
  cor_relCI[s] <-cor(ciz.dat$relCIW_avg.x,ciz.dat$relCIW_avg.y,use="na.or.complete")
  cor_survZ[s] <- cor(ciz.dat$Sinclair_Z.x,
                      ciz.dat$Sinclair_Z.y,
                      use="na.or.complete")
  cor_fall_caa_Z[s] <-cor(ciz.dat$Sinclair_Z.x,
                         ciz.dat$CAA_Z.x,
                         use="na.or.complete")
  cor_spring_caa_Z[s] <-cor(ciz.dat$Sinclair_Z.y,
                          ciz.dat$CAA_Z.y,
                          use="na.or.complete")

    
  plots[[s]] <-ggplot(pdat,aes(x=relCIW_avg, y=Sinclair_Z))+
    geom_smooth(method='lm',se=F,color="black")+
   geom_point(na.rm = TRUE,aes(shape=survey,colour=year_cat),size=1)+
    scale_color_manual(values = c("black", "gray") )+
    scale_shape_manual(values=c(19,3))+
    #xlab("Relative catch / index") +
    #ylab("Total mortality (Z)") +
    xlab("") +
    ylab("") +
    ggtitle(stock.names[s])+
    coord_cartesian(xlim=xlims,ylim=c(0,2))+
    #scale_x_continuous(limits = xlims)+
    #scale_y_continuous(limits = c(0, 2))+
    #facet_wrap("spe")
    expand_limits(y=min(0,min(pdat$Sinclair_Z,na.rm=T)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          text=element_text(size=8,  family="Times"),
          legend.position = "none",
          plot.title = element_text(hjust = 0.)) 
  
  
  # spd <- filter(pdat,survey=="NMFS spring BTS")
  # lmsp <- lm(spd$Sinclair_Z~spd$relCIW_avg)
  # slope_spring[s] <- lmsp$coefficients[2]
  # 
  # fd <- filter(pdat,survey=="NMFS fall BTS")
  # lmf <- lm(spd$Sinclair_Z~spd$relCIW_avg)
  # slope_fall[s] <- lmf$coefficients[2]
  # 
  
  # sp_plots[[s]] <-ggplot(spd,aes(x=relCIW_avg, y=Sinclair_Z, color = year_cat))+
  #   #geom_line(data=fortify(lmsp), aes(x = relCIW_avg, y = .fitted))+
  #   #geom_smooth(method='lm',se=F,color="black",line)+
  #   geom_point(na.rm = TRUE,aes(colour=year_cat))+
  #   scale_color_manual(values = c("black", "gray"))+
  #   #xlab("Relative catch / index") +
  #   #ylab("Total mortality (Z)") +
  #   xlab("") +
  #   ylab("") +
  #   ggtitle(stock.names.spring[s])+
  #   coord_cartesian(xlim=xlims,ylim=c(0,2))+
  #   #scale_x_continuous(limits = xlims)+
  #   #scale_y_continuous(limits = c(0, 2))+
  #   #facet_wrap("spe")
  #   #expand_limits(y=min(0,min(spd$Sinclair_Z,na.rm=T)))+
  #   theme_bw()+
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         strip.background = element_blank(),
  #         panel.border = element_rect(colour = "black"),
  #         text=element_text(size=8,  family="Times"),
  #         legend.position = "none",
  #         plot.title = element_text(hjust = 0.)) 
  # 
  
  # f_plots[[s]] <-ggplot(sdat,aes(x=relCIW_avg, y=Sinclair_Z))+
  #   geom_smooth(method='lm',se=F,color="black",line)+
  #   geom_point(na.rm = TRUE,aes(colour=year_cat))+
  #   scale_color_manual(values = c("black", "gray") )+
  #   #xlab("Relative catch / index") +
  #   #ylab("Total mortality (Z)") +
  #   xlab("") +
  #   ylab("") +
  #   ggtitle(stock.names.fall[s])+
  #   coord_cartesian(xlim=xlims,ylim=c(0,2))+
  #   #scale_x_continuous(limits = xlims)+
  #   #scale_y_continuous(limits = c(0, 2))+
  #   #facet_wrap("spe")
  #   expand_limits(y=min(0,min(pdat$Sinclair_Z,na.rm=T)))+
  #   theme_bw()+
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         strip.background = element_blank(),
  #         panel.border = element_rect(colour = "black"),
  #         text=element_text(size=8,  family="Times"),
  #         legend.position = "none",
  #         plot.title = element_text(hjust = 0.)) 
  
  
  
  
  
  cz.plots[[s]] <-ggplot(pdat,aes(x=relCIW_avg, y=CAA_Z))+
    geom_smooth(method='lm',se=F,color="black")+
    geom_point(na.rm = TRUE,aes(colour=year_cat))+
    scale_color_manual(values = c("black", "gray") )+
    #xlab("Relative catch / index") +
    #ylab("Total mortality (Z)") +
    xlab("") +
    ylab("") +
    ggtitle(stock.names[s])+
    #coord_cartesian(xlim=xlims,ylim=c(0,2))+
    scale_x_continuous(limits = xlims)+
    #scale_y_continuous(limits = c(0, 2))+
    #facet_wrap("spe")
    expand_limits(y=min(0,min(pdat$CAA_Z,na.rm=T)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          text=element_text(size=8,  family="Times"),
          legend.position = "none",
          plot.title = element_text(hjust = 0.)) 
  
}

#df_table <- as.data.frame(cbind(stocks,Z_pre,Z_post,Zrat,relCI_pre,relCI_post,CIrat))
quartz()

pdf(file = ".//figs//Figure_3.pdf",height=10,width=8)
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],
plots[[5]],plots[[6]],plots[[7]],plots[[8]],
plots[[9]],plots[[10]],plots[[11]],plots[[12]],
plots[[13]],plots[[14]],plots[[15]],plots[[16]],
plots[[17]],plots[[18]],    nrow=6)
dev.off()

pdf(file = ".//figs//Figure_3_CAA_Z.pdf",height=10,width=8)
grid.arrange(cz.plots[[1]],cz.plots[[2]],cz.plots[[3]],cz.plots[[4]],
             cz.plots[[5]],cz.plots[[6]],cz.plots[[7]],cz.plots[[8]],
             cz.plots[[9]],cz.plots[[10]],cz.plots[[11]],cz.plots[[12]],
             cz.plots[[13]],cz.plots[[14]],cz.plots[[15]],cz.plots[[16]],
             cz.plots[[17]],cz.plots[[18]],    nrow=6)
dev.off()

pdf(file = ".//figs//Figure_3_cohort_Z.pdf",height=10,width=8)
grid.arrange(coh.plots[[1]],coh.plots[[2]],coh.plots[[3]],coh.plots[[4]],
             coh.plots[[5]],coh.plots[[6]],coh.plots[[7]],coh.plots[[8]],
             coh.plots[[9]],coh.plots[[10]],coh.plots[[11]],coh.plots[[12]],
             coh.plots[[13]],coh.plots[[15]],coh.plots[[16]],
             coh.plots[[17]],coh.plots[[18]],    nrow=6)
dev.off()


# Make a table of the correlations
cor.df <- as.data.frame(cbind(stocks,relCI_pre,relCI_post,CIrat,Zs_pre,Zs_post,Zrat,Zf_pre,Zf_post,
                              cor_relCI,cor_CIZs,cor_CIZs_spring,cor_CIZs_fall,cor_CIZf,cor_coh_CIZ,cor_survZ,cor_fall_caa_Z,cor_spring_caa_Z),stringsAsFactors = FALSE)
colnames(cor.df)=c("stock","relCI_pre","relCI_post","relCI_rat","Zs_pre","Zs_post","Zf_pre","Zf_post","Z_rat",
                   "cor_relCI","CIZs","CIZs_spring","CIZs_fall","CIZf","CIZc","survZ","fall_caa_Z","spring_caa_Z")
cor.df <- merge(cor.df,rho,by="stock",all=TRUE)
cor.df$CIZs <- as.numeric(cor.df$CIZs)
cor.df$CIZf <- as.numeric(cor.df$CIZf)
cor.df$CIZc <- as.numeric(cor.df$CIZc)
cor.df$stock <- ordered(cor.df$stock,levels<-stocks)
cor.df <-cor.df %>% slice(match(stocks, stock))
write.csv(cor.df,"Correlation_table.csv")
cor.df$cor_ind=1
cor.df$cor_ind[cor.df$CIZs >=0.3] = 0


pdf(file = ".//figs//Figure_4_3_panels.pdf",height=7,width=7)
p4.1<-ggplot(cor.df, aes(x=CIZs,y=CIZf,label=stock_abbr))+
  theme_classic(base_family = "Times New Roman",base_size = 12)+
  geom_point()+
  geom_text(nudge_x=0.11)+
  #geom_label(aes(group=stock_abbr,label=stock_abbr),color="black",size=2,family="Times New Roman",fontface="bold")+
  geom_abline(linetype=2)+
  xlab("Correlation between relative F and survey Z") +
  ylab("Correlation between relative F and fishery Z")
  #ggtitle("A")
  
p4.2<-ggplot(cor.df, aes(x=CIZs,y=CIZc,label=stock_abbr))+
  theme_classic(base_family = "Times New Roman",base_size = 12)+
  geom_point()+
  geom_text(nudge_x=0.11)+
  #geom_label(aes(group=stock_abbr,label=stock_abbr),color="black",size=2,family="Times New Roman",fontface="bold")+
  geom_abline(linetype=2)+
  xlab("Correlation between relative F and survey Z") +
  ylab("Correlation between cohort relative F and cohort Z")+
  ggtitle("B")

  p4.3<-ggplot(cor.df, aes(x=CIZs,y=rho,label=stock_abbr))+
    theme_classic(base_family = "Times New Roman",base_size = 12)+
    geom_point()+
    geom_text(nudge_x=0.11)+
    #geom_label(aes(group=stock_abbr,label=stock_abbr),color="black",size=2.,family="Times New Roman",fontface="bold")+
    xlab("Correlation between relative F and survey Z") +
    ylab("Mohn's rho")
    #ggtitle("C")
  
  quartz()
  p4.3
  quartz()
  p4.1
  grid.arrange(p4.1,p4.2,p4.3,nrow=1)
  
dev.off()

pdf(file = ".//figs//Figure_4_2_panels.pdf",height=7,width=7)
p4.1<-ggplot(cor.df, aes(x=CIZs,y=CIZf,label=stock_abbr))+
  theme_classic(base_family = "Times New Roman",base_size = 12)+
  geom_point()+
  geom_text(nudge_x=0.09)+
  #geom_label(aes(group=stock_abbr,label=stock_abbr),color="black",size=2,family="Times New Roman",fontface="bold")+
  geom_abline(linetype=2)+
  xlab("Correlation between relative F and survey Z") +
  ylab("Correlation between relative F and fishery Z")+
  ggtitle("A")

p4.2<-ggplot(cor.df, aes(x=CIZs,y=rho,label=stock_abbr))+
  theme_classic(base_family = "Times New Roman",base_size = 12)+
  geom_point()+
  geom_text(nudge_x=0.09)+
  #geom_label(aes(group=stock_abbr,label=stock_abbr),color="black",size=2.,family="Times New Roman",fontface="bold")+
  xlab("Correlation between relative F and survey Z") +
  ylab("Mohn's rho")+
  ggtitle("B")

quartz()
grid.arrange(p4.1,p4.2,nrow=1)

dev.off()


pdf(file = ".//figs//Figure_4_1_panel.pdf",height=7,width=7)
p4<-ggplot(cor.df, aes(x=CIZs,y=rho,label=stock_abbr))+
  theme_classic(base_family = "Times New Roman",base_size = 14)+
  geom_point()+
  geom_text(nudge_x=0.035)+
  #geom_label(aes(group=stock_abbr,label=stock_abbr),color="black",size=2.,family="Times New Roman",fontface="bold")+
  xlab("Correlation between relative F and survey Z") +
  ylab("Mohn's rho")
  #ggtitle("B")
  quartz()
  print(p4)
dev.off()



#df95 <- filter(resdf,year>=1995,year.avg==4,year.mod==1)
df95 <- Avg_95
for(s in 1:18)
{
  stock.name = stocks[s]
  df95$Zmu[df95$stock==stock.name]= Avg_94$Z_94[Avg_94$stock==stock.name]
  df95$CIWmu[df95$stock==stock.name]= Avg_94$relCIW_94[Avg_94$stock==stock.name]
  df95$CINmu[df95$stock==stock.name]= Avg_94$relCIN_94[Avg_94$stock==stock.name]
}

df95$Zrat <- df95$Z_yr/df95$Zmu
df95$CINrat <- df95$relCIN_yr/df95$CINmu
df95$CIWrat <- df95$relCIW_yr/df95$CIWmu
df95$Cmult <- df95$Zrat/df95$CIWrat
df95$Cmult[df95$stock=="herring"] <- df95$Zrat[df95$stock=="herring"]/df95$CINrat[df95$stock=="herring"]
df95$newC <- df95$Cmult * df95$Ctot
df95$Cdif <- df95$newC - df95$Ctot

# create a table of the mean % diff, the mean catch over time and the total catch across years

cd<- as.data.frame.table(tapply((df95$Cmult),list(df95$stock),mean,na.rm=TRUE))
colnames(cd) <-c("stock","rel_dC")
cd$stock =  as.factor(levels(cd$stock))[cd$stock]
cd$dC_mu<- as.vector(tapply(df95$Cdif,list(df95$stock),mean,na.rm=TRUE))
cd$dC_max<- as.vector(tapply(df95$Cdif,list(df95$stock),max,na.rm=TRUE))
cd$dC_tot<- as.vector(tapply(df95$Cdif,list(df95$stock),sum,na.rm=TRUE))







#regions <- c("GOM","GB","SNEMA","MA","Pelagic")
yr<- 5
cols = c("black","gray")
ylab = "Sinclair Z"
xlab = "Realtive catch / index"
psize <- 1.5
quartz()
spec <- "GBcod"
 gbcod<- ggplot(filter(resdf,year.avg==yr,stock==spec),aes(x=relCIW_avg, y=Sinclair_Z, color=year_cat))+
  geom_point(na.rm = TRUE,aes(shape=survey),size=psize)+
  scale_color_manual(values = cols )+
  xlab(xlab) +
  ylab(ylab) +
  theme_bw()+
   ggtitle("GB cod")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=12,  family="Times"),
        legend.position = "none")  
print(gbcod)

spec <- "GOMcod"
gomcod<- ggplot(filter(resdf,year.avg==yr,stock==spec),aes(x=relCIW_avg, y=Sinclair_Z, color=year_cat))+
  geom_point(na.rm = TRUE,aes(shape=survey),size=psize)+
  scale_color_manual(values = cols )+
  xlab(xlab) +
  ylab(ylab) +
  theme_bw()+
  ggtitle("GB cod")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=12,  family="Times"),
        legend.position = "none")  
print(gomcod)

yr<- 5
spec <- "GBhaddock"
gbhad<- ggplot(filter(resdf,year.avg==yr,stock==spec),aes(x=relCIW, y=Sinclair_Z, color=year_cat))+
  geom_point(na.rm = TRUE,aes(shape=survey))+
  scale_color_manual(values = c("red", "blue") )+
  xlab("Relative catch / index (5 year avg)") +
  ylab("Sinclair Z (5 yr)") +
  ggtitle("GB haddock")+
  theme(plot.title = element_text(hjust = 1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=12,  family="Times"))  
print(gbhad)





pdf(file = ".//figs//rel_CIN_by_region.pdf")
{
  quartz()
  p <- ggplot(filter(df_avg, year.avg==5,region!="Pelagic"), aes(x=year, y=relCIW_avg, color=species)) +
    #geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE,aes(linetype=survey)) +
    geom_line(data=filter(df_avg,stock=="Average",year.avg==5,region!="Pelagic"),
              color="black",size=1.25,aes(x=year,y=relCIW_savg))+
     coord_cartesian(xlim=c(1978,2012))+
    scale_x_continuous(limits = c(1978, 2012))+
    facet_wrap(c("region"),nrow=3) +
    xlab("Year") +
    ylab("Relative catch / index (5 year avg)") +
    #ggtitle(this_region) +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          text=element_text(size=12,  family="Times"))
    
  print(p)
}
dev.off()






scaleFUN <- function(x) sprintf("%.1f", x)
resdf2 <- resdf
#resdf2 <- resdf2[-c(3327:3340,3451:3464),]
resdf2 <- resdf2[!(resdf2$stock=="herring" & resdf2$year<=1988),]
d<-d[!(d$A=="B" & d$E==0),]
quartz()

colnames(resdf2)[which(names(resdf2) == "year_cat")] <- "years"

pdf(file = ".//figs//new_Fig4.pdf")
{
p<- ggplot(filter(resdf2,year.avg==4),aes(x=relCIW_avg, y=Sinclair_Z, color=years))+
  geom_point(na.rm = TRUE,aes(shape=survey))+
  geom_smooth(method='lm',se=F,color="black")+
  scale_color_manual(values = c("gray", "black") )+
  xlab("Relative F") +
  ylab("Total mortality (Z)") +
  scale_y_continuous(labels=scaleFUN,breaks=c(0,0.5,1.0,1.5,2.0,2.5,3.0))+
  facet_wrap("plot.name",scales="free",ncol=3)+
  #geom_blank(aes())
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=10,  family="Times"),
        plot.title = element_text(hjust=1))  
print(p)
}
dev.off()

quartz()
p<- ggplot(filter(resdf,year.avg==5),aes(x=relCIN_avg, y=Sinclair_Z, color=year_cat))+
  geom_point(na.rm = TRUE,aes(shape=survey))+
  scale_color_manual(values = c("red", "blue") )+
  xlab("Relative catch / index (5 year avg)") +
  ylab("Sinclair Z (5 yr)") +
  facet_wrap("stock")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=12,  family="Times"))  
print(p)


p <- ggplot(filter(resdf, stock == "fluke"|stock=="scup",year.avg==5), aes(x=relCIW, y=Sinclair_Z, color=year_cat)) +
  geom_point(na.rm = TRUE)+
  facet_grid(survey~species)+


quartz()
p <- ggplot(filter(resdf, stock == "GBwinter"|stock=="SNEMAwinter",year.avg==5), aes(x=relCIW, y=Sinclair_Z, color=year_cat)) +
  geom_point(na.rm = TRUE)+
  scale_color_manual(values = c("red", "blue") )+
  facet_grid(survey~stock)+
  xlab("Relative catch / index (5 year avg)") +
  ylab("Sinclair Z (5 yr)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=12,  family="Times"))  
print(p)

quartz()
p <- ggplot(filter(resdf, stock == "GOMcod"|stock=="CCGOMyt",year.avg==5), aes(x=relCIW, y=Sinclair_Z, color=year_cat)) +
  geom_point(na.rm = TRUE)+
  scale_color_manual(values = c("red", "blue") )+
  facet_grid(survey~stock)+
  xlab("Relative catch / index (5 year avg)") +
  ylab("Sinclair Z (5 yr)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=12,  family="Times"))  
print(p)


quartz()
p <- ggplot(filter(resdf, stock == "witch"|stock=="GByt",year.avg==5), aes(x=relCIW, y=Sinclair_Z, color=year_cat)) +
  geom_point(na.rm = TRUE)+
  scale_color_manual(values = c("red", "blue") )+
  facet_grid(survey~species)+
  xlab("Relative catch / index (5 year avg)") +
  ylab("Sinclair Z (5 yr)") +
  scale_y_continuous(limits = c(0, 2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=12,  family="Times"))  
print(p)














quartz()
p <- ggplot(filter(df_avg, year.avg==5,stock!="Average"), aes(x=year,y=relCIN_avg)) +
  geom_line(na.rm=TRUE,aes(linetype=survey,color=stock))+
  geom_line(data=filter(df_avg, year.avg==5,stock=="Average"),
            aes(x=year,y=relCIN_savg,linetype=survey),color="black",na.rm=TRUE,size=1.25)+
  #scale_x_continuous(limits = c(1978, 2014))+
  coord_cartesian(xlim=c(1978,2014))+
  facet_wrap(c("region"))
p



# plot 

pdf(file = ".//figs//rel_CIW_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=relCIW_avg)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
    facet_wrap(c("survey","year.avg")) +
    xlab("Year") +
    ylab("Relative catch / index (3 year avg)") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()


pdf(file = ".//figs//SinZ_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=Sinclair_Z)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
    facet_wrap(c("survey","year.avg")) +
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
pdf(file = ".//figs//CAA_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock, year.avg==4), aes(x=year, y=CAA_Z)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    geom_ribbon(aes(ymin=low90_CAA_Z, ymax=high90_CAA_Z), alpha=0.3) +
    #facet_wrap(~survey) +
    xlab("Year") +
    ylab("Fishery catch-at-age Z") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()

pdf(file = ".//figs//Cohort_CI_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock, year.avg==3), aes(x=year, y=cohort_CI)) +
    #geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_CAA_Z, ymax=high90_CAA_Z), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Year") +
    ylab("Cohort CI") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()

pdf(file = ".//figs//CAA_Z_vs_survey_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock, year.avg==3), aes(x=CAA_Z,Sinclair_Z,label=year)) +
    geom_point(na.rm = TRUE) +
    geom_abline() +
    geom_text()+
    #geom_ribbon(aes(ymin=low90_CAA_Z, ymax=high90_CAA_Z), alpha=0.3) +
    facet_wrap(~survey) +
    ylab("Survey Z") +
    xlab("Fishery Z") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()



s=1
GBCdat_m1 <- filter(resdf,year.avg==3,year.mod==0,stock=="GBcod",rel_cohCI<3)
lmcoh <- lm(p3dat$cohort_Z~p3dat$rel_cohCI)
cor_coh_CIZ[s]<-cor(GBCdat_m1$rel_cohCI,GBCdat_m1$cohort_Z,use="na.or.complete")
slope_cohCIZ[s] <-lm1$coefficients[2]

coh.plots[[s]] <-ggplot(p3dat,aes(x=rel_cohCI, y=cohort_Z))+
  geom_smooth(method='lm',se=F,color="black")+
  geom_point(na.rm = TRUE, aes(shape=survey))+
  #scale_color_manual(values = c("black", "gray") )+
  #xlab("Relative catch / index") +
  #ylab("Total mortality (Z)") +
  xlab("") +
  ylab("") +
  ggtitle(stock.names[s])+
  coord_cartesian(xlim=c(0,5.5),ylim=c(0,3))+
  scale_x_continuous(limits = c(0,5.5))+
  scale_y_continuous(limits = c(0, 3))+
  #facet_wrap("spe")
  expand_limits(y=min(0,min(pdat$cohort_Z,na.rm=T)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        text=element_text(size=8,  family="Times"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.)) 





########## Figure 2 cohort C/I  by Region  ######

pdf(file = ".//figs//Avg_cohort_CI_by_region.pdf")
{
  
  p2.CI.all<- ggplot(filter(df_avg, year.avg==3,region!="Pelagic"), aes(x=year, y=rel_cohCI, color=species)) +
    geom_line(na.rm = TRUE,aes(linetype=survey)) +
    geom_line(data=filter(df_avg,stock=="Average",region!="Pelagic"),
              color="black",size=1.25,aes(x=year,y=rel_cohCI_region))+
    coord_cartesian(xlim=c(1978,2010),ylim=c(0,4))+
    scale_x_continuous(limits = c(1978, 2010))+
    
    facet_wrap(c("plot_region"),nrow=3) +
    xlab("Cohort year") +
    ylab("Relative F") +
    theme_bw()+
    #theme_classic(base_family = "Times New Roman",base_size = leg.size)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          text=element_text(size=12,  family="Times"),strip.text = element_text(size = 12, hjust = 0))
  quartz()
  print(p2.CI.all)
}
dev.off()


# this is print out the years to see which is the first year with complete data
for(ss in 1:18)
{
  sname <- stocks[ss]
  tmp<- resdf[resdf$stock==sname & resdf$year >=1978,c(1:3,5,14,15,19)]
  #tmp<-tmp[!is.na(tmp$CIW)]
  print(head(tmp,30))
}

tdf <- NULL 
for(ss in 1:18)
{
  sname <- stocks[ss]
  C_bef<- ctot.dat$catch[ctot.dat$stock==sname & ctot.dat$year >=1978 & ctot.dat$year<1995]
  C_aft <- ctot.dat$catch[ctot.dat$stock==sname & ctot.dat$year >=1995 & ctot.dat$year<2015]
  
  Cadj_bef <- C_bef * (1-CIrat[ss])
  #print(cbind(ss,Cadj_bef,(C_bef-(C_bef*CIrat[ss]))))
  Cadj_aft <- C_aft/CIrat[ss] - C_aft
  #print(cbind(ss,Cadj_aft,(C_aft * (1/CIrat[ss]-1))))
  
  muC_bef <-mean(Cadj_bef,na.rm=TRUE)
  minC_bef <- min(Cadj_bef,na.rm=TRUE)
  maxC_bef <- max(Cadj_bef,na.rm=TRUE)
  muC_aft <-mean(Cadj_aft,na.rm=TRUE)
  minC_aft <- min(Cadj_aft,na.rm=TRUE)
  maxC_aft <- max(Cadj_aft,na.rm=TRUE)
  
  byr <- seq(1978,1994,1)
  ayr <- seq(1995,2014,1)
  
  oname <- "Overestimated catches"
  uname <- "Underestimated catches"
  
  bdf <- as.data.frame(cbind(sname,byr,oname,C_bef,Cadj_bef,cor_CIZs[ss],CIrat[ss],muC_bef,minC_bef,maxC_bef,muC_aft,minC_aft,maxC_aft),stringsAsFactors=FALSE)
  adf <- as.data.frame(cbind(sname,ayr,uname,C_bef,Cadj_bef,cor_CIZs[ss],CIrat[ss],muC_bef,minC_bef,maxC_bef,muC_aft,minC_aft,maxC_aft),stringsAsFactors=FALSE)
  colnames(bdf) <-colnames(adf) <- c("stock","year","OU","Catch","Adj_catch","cor","CIrat","muC_bef","minC_bef","maxC_bef","muC_aft","minC_aft","maxC_aft")
  tdf <- rbind(tdf,bdf,adf)
  #print(head(tmp,30))
}

region.stocks <- c( "GBcod" , "GBhaddock", "GByt","GBwinter", 
                    "GOMcod", "GOMhaddock",  "CCGOMyt", "witch", "plaice", "pollock", "whitehake", "redfish",
                    "SNEMAyt",  "SNEMAwinter",  "fluke", "scup",       
                     "mackerel", "herring")

tdf$cor <- as.numeric(tdf$cor)
tdf$cor_ind <-0
tdf$cor_ind[tdf$cor_ind <=0.3] <-1
tdf$Catch <- as.numeric(tdf$Catch)
tdf$Adj_catch <- as.numeric(tdf$Adj_catch)
tdf.out <-tdf %>% slice(match(region.stocks, stock))
tdf.out[,c(1,6,8,9,10,11,12,13)]
#tdf %>% filter(stock=="CCGOMyt")
bd <- tdf[tdf$OU==oname & tdf$cor_ind==1,]
ad <- tdf[tdf$OU==uname & tdf$cor_ind==1,]

#sapply(tdf, class)
quartz()
pd <- tdf[tdf$cor_ind==1,]
boxplot(pd$Adj_catch~pd$OU+pd$stock)

