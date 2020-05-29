# get_Sinclair_Z.R
# estimate total mortality rate from survey catch at age data in ADIOS format using Sinclair method

library("ASAPplots")
library("dplyr")
library("ggplot2")

decoder <- read.csv(".//ADIOS_data//file_decoder.csv")
nstocks <- length(decoder$Short.Name)

# create default list of stocks, surveys, usesurvey (T/F), start age, end age, first year, last year
defdf <- data.frame(stockID = integer(),
                    stock = character(),
                    survey = character(),
                    usesurvey = logical(),
                    startage = integer(),
                    endage = integer(),
                    firstyear = integer(),
                    endyear = integer(),
                    note = character())
for (istock in 1:nstocks){
  #dat <- read.csv(paste0(".\\ADIOS_data\\", decoder$ADIOS.name[istock], ".csv"))
  dat <- read.csv(paste0(".//ADIOS_data//", decoder$ADIOS.name[istock], ".csv"))
  surveys <- unique(dat$SURVEY)
  nsurveys <- length(surveys)
  for (isurvey in 1:nsurveys){
    sdat <- dat %>%
      filter(SURVEY == surveys[isurvey])
    minyear <- min(sdat$YEAR)
    maxyear <- max(sdat$YEAR)
    minage <- min(sdat$AGE)
    maxage <- max(sdat$AGE)
    thisdf <- data.frame(stockID = istock,
                         stock = decoder$Short.Name[istock],
                         survey = as.character(surveys[isurvey]),
                         usesurvey = TRUE,
                         startage = minage,
                         endage = maxage,
                         firstyear = minyear,
                         endyear = maxyear,
                         note = "original")
    defdf <- rbind(defdf, thisdf)
  }
}
defdf$rowID <- seq(1, length(defdf[,1]))
defdf
#write.csv(defdf, file=".\\ADIOS_data\\survey_options_orig.csv", row.names = FALSE)
write.csv(defdf, file=".//ADIOS_data//survey_options_orig.csv", row.names = FALSE)

#############################################################################################
# need to do some work by hand outside this program - described in this block of comments
# save survey_options_use.csv only when run the first time
# write.csv(defdf, file=".\\ADIOS_data\\survey_options_use.csv", row.names = FALSE)
# now run Shiny app.R and change survey_options_use.csv for each stock/survey combination
# note survey_options_use.csv will be overwritten if click Update button - so be careful
# survey_options_use_backup.csv provided in case accidently overwrite the file
# Shiny app located in directory ./inst/shiny_examples/SetUp/SinclairZ
#############################################################################################

# Estimate Sinclair Z based on age ranges for used surveys only and plot
#so_use <- read.csv(".\\ADIOS_data\\survey_options_use.csv", stringsAsFactors = FALSE)
so_use <- read.csv(".//ADIOS_data//survey_options_use.csv", stringsAsFactors = FALSE)
res <- list()
resdf <- data.frame()
for (istock in 1:1){
  #dat <- read.csv(paste0(".\\ADIOS_data\\", decoder$ADIOS.name[istock], ".csv"))
  dat <- read.csv(paste0(".//ADIOS_data//", decoder$ADIOS.name[istock], ".csv"))
  surveys <- unique(dat$SURVEY)
  nsurveys <- length(surveys)
  res[[istock]] <- list()
  res[[istock]]$stock <- decoder$Short.Name[istock]
  res[[istock]]$surveys <- surveys
  res[[istock]]$z <- list()
  for (isurvey in 1:nsurveys){
    myrow <- filter(so_use, stock == decoder$Short.Name[istock], survey == surveys[isurvey]) %>%
      select(rowID) %>%
      as.numeric(.)
    if (so_use$usesurvey[myrow] == TRUE){
      sdat <- dat %>%
        filter(SURVEY == surveys[isurvey]) %>%
        filter(AGE %in% seq(so_use$startage[myrow], so_use$endage[myrow]))
      all.sdat <-  dat %>%
        filter(SURVEY == surveys[isurvey]) 
        
      minyear <- min(sdat$YEAR)
      maxyear <- max(sdat$YEAR)
      minage.all<-max(1,min(all.sdat$AGE))
      minage <- min(sdat$AGE)
      maxage <- max(sdat$AGE)
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
      
      myres.sin <- calc_Sinclair_Zmod(mat,3)
      myres.coh <- calc_cohort_Z(mat,8)
      myres.sin.dynfa <- calc_Sinclair_Z_dynfa(all.mat,3)
      
      res[[istock]]$z.sin[[isurvey]] <- myres.sin
      res[[istock]]$z.coh[[isurvey]] <- myres.coh
      
      if (myres$error == FALSE & !all(is.na(myres$est.Sinclair.Z))){
        sin.df <- data.frame(stock = decoder$Short.Name[istock],
                             survey = as.character(surveys[isurvey]),
                             year = rep(myres.sin$plot.year, 1),
                             Sinclair_Z = myres.sin$est.Sinclair.Z[,1],
                             low90_Sinclair = myres.sin$est.Sinclair.Z[,2],
                             high90_Sinclair = myres.sin$est.Sinclair.Z[,3])
        
        sin.df.dynfa <- data.frame(stock = decoder$Short.Name[istock],
                             survey = as.character(surveys[isurvey]),
                             year = rep(myres.sin.dynfa$plot.year, 1),
                             Sinclair_Z_dynfa = myres.sin.dynfa$est.Sinclair.Z[,1],
                             low90_Sinclair_dynfa = myres.sin.dynfa$est.Sinclair.Z[,2],
                             high90_Sinclair_dynfa = myres.sin.dynfa$est.Sinclair.Z[,3])
                            
        coh.df <- data.frame(stock = decoder$Short.Name[istock],
                             survey = as.character(surveys[isurvey]),
                             year = rep(myres.coh$plot.year,1),
                             cohort_Z = myres.coh$est.cohort.Z[,1],
                             low90_cohort = myres.coh$est.cohort.Z[,2],
                             high90_cohort = myres.coh$est.cohort.Z[,3])
                             
        mdf <- merge(sin.df,sin.df.dynfa,by=c("stock","survey","year"))
        mdf <- merge(mdf,coh.df,by=c("stock","survey","year"))
        resdf <- rbind(resdf, mdf)
      }
    }
  }
}

resdf
year_breaks <- c(1990,2000)
resdf$year_cat <-"1989 and before"
resdf$year_cat[resdf$year>year_breaks[1]] <-"1990 to 1999"
resdf$year_cat[resdf$year>=year_breaks[2]] <-"2000 to present"


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
    ylab("Relative catch / index") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()

pdf(file = ".//figs//CI_vs_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=relCI_3yr, y=Sinclair_Z_dynfa, color=year_cat)) +
    geom_point(na.rm = TRUE) +
    #geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Relative catch / index (3 year") +
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

# Sinclair Z plots
pdf(file = ".//figs//cohort_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=cohort_Z)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    #geom_ribbon(aes(ymin=low90_cohort, ymax=high90_cohort), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Year of cohort birth") +
    ylab("Cohort Z") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()

pdf(file = ".//figs//CAA_Z_plots.pdf")
use.stocks = nstocks
for (istock in 1:use.stocks){
  thisstock <- decoder$Short.Name[istock]
  p <- ggplot(filter(resdf, stock == thisstock), aes(x=year, y=CAA_Z)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    geom_ribbon(aes(ymin=low90_CAA, ymax=high90_CAA), alpha=0.3) +
    facet_wrap(~survey) +
    xlab("Year") +
    ylab("Fishery catch-at-age Z") +
    ggtitle(thisstock) +
    theme_bw()
  print(p)
}
dev.off()


# change it so it has an use.years arguments
calc_Sinclair_Zmod <- function (mat,use.years) 
{
  res <- list()
  mat <- as.matrix(mat)
  if (all(is.na(mat))) {
    res$error <- TRUE
    return(res)
  }

  year <- as.numeric(rownames(mat))
  age <- as.numeric(colnames(mat))
  ny <- length(year)
  est.Sinclair.Z <- matrix(NA, nrow = (ny - (use.years-1)), ncol = 3)
  plot.year <- rep(NA, ny - (use.years-1))
  for (i in 1:(ny - (use.years-1))) {
    submat <- mat[year %in% year[i:(i + (use.years-1))], ]
    data1 <- reshape2::melt(submat)
    colnames(data1) <- c("Year", "Age", "Value")
    data1$cohort <- data1$Year - data1$Age
    data1$Value[data1$Value == 0] <- NA
    data1 <- data1[!is.na(data1$Value), ]
    tdd <<-data1
    data1$lnVal <- log(data1$Value)
    can.calc <- FALSE
    if (length(data1[, 1]) >= 2) {
      if (max(table(data1$cohort)) >= 3) {
        can.calc <- TRUE
      }
    }
    if (can.calc == TRUE) {
      my.lm <- lm(data1$lnVal ~ as.factor(data1$cohort) + 
                    data1$Age)
      data1$pred <- predict(my.lm)
      data1$resid <- residuals(my.lm)
      res[[i]] <- data1
      est.Sinclair.Z[i, 1] <- -1 * my.lm$coefficients[names(my.lm$coefficients) == 
                                                        "data1$Age"]
      est.Sinclair.Z[i, 2:3] <- -1 * rev(confint(my.lm, 
                                                 "data1$Age", level = 0.9))
    }
    else {
      res[[i]] <- data1
      est.Sinclair.Z[i, ] <- rep(NA, 3)
    }
    plot.year[i] <- year[i] + (use.years/2)-0.5
  }
  colnames(est.Sinclair.Z) <- c("Sinclair_Z", "low90%_Sinclair", "high90%_Sinclair")
  res$est.Sinclair.Z <- est.Sinclair.Z
  res$plot.year <- plot.year
  res$error <- FALSE
  return(res)
}


calc_Sinclair_Z_dynfa <- function (mat,use.years) # Sinclair Z with a dynamic first age
{
  res <- list()
  mat <- as.matrix(mat)
  if (all(is.na(mat))) {
    res$error <- TRUE
    return(res)
  }
  year <- as.numeric(rownames(mat))
  age <- as.numeric(colnames(mat))
  ny <- length(year)
  est.Sinclair.Z <- matrix(NA, nrow = (ny - (use.years-1)), ncol = 3)
  plot.year <- rep(NA, ny - (use.years-1))
  afs <- rep(NA, ny - (use.years-1))
    
  for (i in 1:(ny - (use.years-1))) {
    submat <- mat[year %in% year[i:(i + (use.years-1))], ]
    n.avg <- apply(submat,2,mean,na.rm=TRUE)
    afs[i]<- age[which.max(n.avg)]
    data1 <- reshape2::melt(submat)
    colnames(data1) <- c("Year", "Age", "Value")
    data1 <- data1[data1$Age>=afs[i],]
    
    data1$cohort <- data1$Year - data1$Age
    data1$Value[data1$Value == 0] <- NA
    data1 <- data1[!is.na(data1$Value), ]
    tdd <<-data1
    data1$lnVal <- log(data1$Value)
    can.calc <- FALSE
    if (length(data1[, 1]) >= 2) {
      if (max(table(data1$cohort)) >= 3) {
        can.calc <- TRUE
      }
    }
    if (can.calc == TRUE) {
      my.lm <- lm(data1$lnVal ~ as.factor(data1$cohort) + 
                    data1$Age)
      data1$pred <- predict(my.lm)
      data1$resid <- residuals(my.lm)
      res[[i]] <- data1
      est.Sinclair.Z[i, 1] <- -1 * my.lm$coefficients[names(my.lm$coefficients) == 
                                                        "data1$Age"]
      est.Sinclair.Z[i, 2:3] <- -1 * rev(confint(my.lm, 
                                                 "data1$Age", level = 0.9))
    }
    else {
      res[[i]] <- data1
      est.Sinclair.Z[i, ] <- rep(NA, 3)
    }
    plot.year[i] <- year[i] + (use.years/2)-0.5
  }
  
  a.out<<-afs
    
  colnames(est.Sinclair.Z) <- c("Sinclair_Z", "low90%_Sinclair", "high90%_Sinclair")
  res$est.Sinclair.Z <- est.Sinclair.Z
  res$plot.year <- plot.year
  res$error <- FALSE
  return(res)
}
calc_Sinclair_Z_dynfa(mat,3)

calc_cohort_Z <- function (mat,drop.years) 
{
  res <- list()
  mat <- as.matrix(mat)
  if (all(is.na(mat))) {
    res$error <- TRUE
    return(res)
  }
  
  #dropY is the number of years to drop at end due to incomplete cohort 
  year <- as.numeric(rownames(mat))
  age <- as.numeric(colnames(mat))
  ny <- length(year)
  est.cohort.Z <- matrix(NA, nrow = (ny), ncol = 3)
  data1 <- reshape2::melt(mat)
  colnames(data1) <- c("Year", "Age", "Value")
  data1$Value[data1$Value == 0] <- NA
  data1$lnVal <- log(data1$Value)
  data1$cohort <- data1$Year - data1$Age
  #td<<-data1
  

  
  for(i in 1:(ny-drop.years))
  #for(i in 1:1)
  {
    cd <<-data1 %>% filter(cohort==year[i])
    
    if(sum(!is.na(cd$lnVal>=3)))
    {
      my.lm <<- lm(cd$lnVal ~ cd$Age)
      est.cohort.Z[i, 1] <- -1 * my.lm$coefficients[names(my.lm$coefficients) =="cd$Age"]
      est.cohort.Z[i, 2:3] <- -1 * rev(confint(my.lm,  "cd$Age", level = 0.9))                                                                             
      
    }
  }
  
  tZ <<- est.cohort.Z
  
  colnames(est.cohort.Z) <- c("cohort_Z", "low90_cohort%", "high90%_cohort")
  res$est.cohort.Z <- est.cohort.Z
  res$plot.year <- year
  res$error <- FALSE
  return(res)
}
  

CCZ_avg <-function(CAA,use.years) 
{
  ny <- dim(CAA)[1]  # number of years
  CAA <- apply(CAA[max(ny - use.years+1, 1):ny, ], 2, mean, na.rm=TRUE) # add up catches in last use.years
  maxageobs <- length(CAA) # maximum age
  
  est.CC.Z <- matrix(NA, nrow = (ny - (use.years-1)), ncol = 3)
  
  AFS <- which.max(CAA) # age @ full selection
  AFS[AFS > (maxageobs - 3)] <- maxageobs - 3  # account for fact that AFS might be the last 2 ages
  #nS <- ceiling(sum(CAA)/2)  # what is this for???
  y <- log(CAA[AFS:maxageobs]/sum(CAA[AFS:maxageobs], na.rm = T)) 
  xc <- 1:length(y)
  y[y == "-Inf"] <- NA
  mod <- lm(y ~ xc)
  chk <- sum(is.na(coef(mod)))
  if (chk) {
    return(NA)
  }
  else {
    coefs <- summary(mod, weights = CAA[AFS:maxageobs])$coefficients[2,1:2]
    #coefs[is.nan(coefs)] <- tiny
    return(-coefs[1])
    #return(-rnorm(reps, coefs[1], coefs[2]))
  }
}

CCZ_sum <-function(CAA,use.years) 
{
  ny <- dim(CAA)[1]  # number of years
  CAA <- apply(CAA[max(ny - use.years+1, 1):ny, ], 2, sum,na.rm=TRUE) # add up catches in last use.years
  maxageobs <- length(CAA) # maximum age
  AFS <- which.max(CAA) # age @ full selection
  AFS[AFS > (maxageobs - 3)] <- maxageobs - 3  # account for fact that AFS might be the last 2 ages
  #nS <- ceiling(sum(CAA)/2)  # what is this for???
  y <- log(CAA[AFS:maxageobs]/sum(CAA[AFS:maxageobs], na.rm = T)) 
  xc <- 1:length(y)
  y[y == "-Inf"] <- NA
  mod <- lm(y ~ xc)
  chk <- sum(is.na(coef(mod)))
  if (chk) {
    return(NA)
  }
  else {
    coefs <- summary(mod, weights = CAA[AFS:maxageobs])$coefficients[2,1:2]
    #coefs[is.nan(coefs)] <- tiny
    return(coefs[1])
    #return(-rnorm(reps, coefs[1], coefs[2]))
  }
}

 

