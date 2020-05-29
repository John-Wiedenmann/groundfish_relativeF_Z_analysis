# Functions used for the grounfish ecosystem work

#calculate a moving average
move.avg <- function(x,n=3){
  
  return(stats::filter(x,rep(1/n,n), sides=2))
  # sides = 2 uses the years on either side of the value
  }

move.avg.2 <- function(x,n=3){
  yrs <- x[,1]
  if(n%%2==0) yrs <- yrs+0.5
  ma <-stats::filter(x[,2],rep(1/n,n), sides=2)
  return(cbind(yrs,ma))
  # sides = 2 uses the years on either side of the value
}


# change it so it has an use.years arguments
calc_Sinclair_Zmod <- function (mat,use.years) 
{
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
    if(all(is.na(submat))) next
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
    if(all(is.na(submat))) next
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
#calc_Sinclair_Z_dynfa(mat,3)

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
  # this applies a linear regression to the averaged index (or catch) at age across use.years
  # then uses the peak year as first in the linear regression
  
  ny <- dim(CAA)[1]  # number of years
  CAA <- apply(CAA[max(ny - use.years+1, 1):ny, ], 2, mean, na.rm=TRUE) # add up catches in last use.years
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


CCZ_sum <-function(CAA,use.years) 
{
  # this applies a linear regression to the summed index (or catch) at age across years
  # then uses the peak year as first in the linear regression
  
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


CC.cohort <-function(CAA) 
{
  # this is currently not used nor has it been tested
  ny <- dim(CAA)[1]  # number of years
  CAA <- CAA[ny, ]
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

calc_cohort_CI <-function(smat,cmat,omit.years=8)
{
  s.ages <- as.numeric(colnames(smat))
  c.ages <- as.numeric(colnames(cmat))
  s.years <- as.numeric(rownames(smat))
  c.years <- as.numeric(rownames(cmat))
  
  crows <- match(s.years,c.years)
  crows <- crows[!is.na(crows)]
  srows <- match(c.years,s.years)
  srows <- srows[!is.na(srows)]
  ccols <- match(s.ages,c.ages)
  ccols <- ccols[!is.na(ccols)]
  scols <- match(c.ages,s.ages)
  scols <- scols[!is.na(scols)]
  
  caa.mat.use<-cmat[crows,ccols]
  if(ncol(cmat) > max(ccols))
  {
    caa.mat.use[,ncol(caa.mat.use)]<-rowSums(cmat[,max(ccols):ncol(cmat)],na.rm=TRUE)
  }
  caa.mat.use <-replace(caa.mat.use,which(caa.mat.use==0),NA)
  
  s.mat.use <-smat[srows,scols]
  if(ncol(smat) > max(ccols))
  {
    s.mat.use[,ncol(s.mat.use)]<-rowSums(smat[srows,max(scols):ncol(smat)],na.rm=TRUE)
  }  
  s.mat.use <-replace(s.mat.use,which(s.mat.use==0),NA)
  
  cdf <- reshape2::melt(caa.mat.use)
  colnames(cdf) <- c("year","age","caa")
  sdf <- reshape2::melt(s.mat.use)
  colnames(sdf) <- c("year","age","naa")
  csdf <- merge(x=cdf,y=sdf,by=c("year","age"))
  csdf$cohort <- csdf$year - csdf$age
  cssum <- as.data.frame.table(tapply(csdf$caa,list(csdf$cohort),sum,na.rm=TRUE))
  colnames(cssum) <-c("year","caa")
  cssum$naa <- as.vector(tapply(csdf$naa,list(csdf$cohort),sum,na.rm=TRUE))
  cssum$year <- as.numeric(levels(cssum$year))[cssum$year]
  
  cssum<-cssum %>% filter(year <=(max(as.numeric(cssum$year))-omit.years+1)) %>%
    filter(year >= min(csdf$year))
  cssum$cohort_CI <- cssum$caa / cssum$naa
  coh.ci <- cssum[,c("year","cohort_CI")]
  return(coh.ci)
  # ci.mat <- caa.mat.use/s.mat.use
  # ci.df <-reshape2::melt(ci.mat)
  # colnames(ci.df) <- c("year", "age", "CI")
  # ci.df$cohort <-ci.df$year - ci.df$age
  # ci.df <- ci.df %>%
  #   filter(cohort <=(max(ci.df$cohort)-omit.years+1)) %>%
  #   filter(cohort >= min(ci.df$year))
  #   
  # ci.df[ci.df$cohort <=(max(ci.df$cohort)-omit.years+1),]
  # coh.ci <- as.data.frame.table(tapply(ci.df$CI,list(ci.df$cohort),mean,na.rm=TRUE))
  # colnames(coh.ci) <-c("year","cohort_CI")
  # coh.ci$cohort_CI[is.nan(coh.ci$cohort_CI)] <- NA
  # return(coh.ci)
}

cpa <- function(min.regime.len, p.level, orig.data.array)
{
  nyears <- length(orig.data.array)
  
  if (!is.numeric(min.regime.len) || !is.numeric(p.level) || nyears < 1 || !is.numeric(orig.data.array))
  {
    return(-1);
  }
  
  if (min.regime.len < 1 || min.regime.len > (nyears / 2))
  {
    min.regime.len <- as.integer(min(10,(min.regime.len / 10)))
  }
  
  if (p.level < 0.0 || p.level > 0.25)
  {
    p.level <- 0.1
  }
  
  # initialize regime indicator array
  
  reg.shift <- array(NA,dim=nyears)
  
  # de-mean the original data
  
  data.array <- orig.data.array - mean(orig.data.array)
  
  
  # using a Student's t test, so find the critical value
  
  sig.t <- qt((1.0 - (p.level / 2.0)),((2 * min.regime.len) - 2),lower.tail=TRUE)
  
  # we assume that the average variance is 1, since sigmaR is set to 1 historically and in the projections
  
  # reg.diff is the statistically significant difference between mean values of two adjacent regimes
  
  reg.diff <- sig.t * sqrt(2.0 * 1.0 / min.regime.len)
  
  
  init.mean <- mean(data.array[1:min.regime.len])
  reg.shift[1] <- 0
  
  # loop over all years of data except the first year
  curr.mean <- init.mean
  for (y in 2:nyears)
  {
    # is there a possible regime shift?
    if (data.array[y] >= (curr.mean - reg.diff) && data.array[y] <= (curr.mean + reg.diff))
    {
      # no regime shift
      if (y < min.regime.len)
      {
        curr.mean <- mean(data.array[1:min.regime.len])
      }
      else
      {
        curr.mean <- mean(data.array[max((y-min.regime.len+1),1):y])
      }
      
      reg.shift[y] <- 0
    }
    else
    {
      # possible regime shift, so check it
      RSI.positive <- TRUE
      
      for (m in 0:min((min.regime.len - 2),(nyears - y)))  # need to truncate this for the end of the data
      {
        RSI.mean <- mean(data.array[y:(y+m)])
        
        RSI.c <- sum(data.array[y:(y+m)] - RSI.mean) / (min.regime.len * 1.0)
        
        if (RSI.c < 0.0)
        {
          RSI.positive <- FALSE
        }
      }
      
      if (RSI.positive)
      {
        # new regime here
        curr.mean <- mean(data.array[(y-1):y])
        reg.shift[y] <- RSI.c
      }
      else
      {
        # no new regime after all
        if (y < min.regime.len)
        {
          curr.mean <- mean(data.array[1:min.regime.len])
        }
        else
        {
          curr.mean <- mean(data.array[max((y-min.regime.len+1),1):y])
        }
        
        reg.shift[y] <- 0
      }
    }
  }
  
  return(reg.shift)
}

# the functions below are translations of Rodionov's code v3.2 from VBA into R
# there is no functionality for addressing autocorrelation in this code

calculate.rsi.attrib <- function(rsi.vector)
{
  reg.mean <- array(0, dim = data.len)
  reg.wtmean <- array(0, dim = data.len)
  reg.length <- array(0, dim = data.len)
  reg.ci <- array(0, dim = data.len)
  
  sample.vector <- array(0.0, dim = reg.len)
  
  sample.var <- array(0.0, dim = (data.len - reg.len + 1))
  
  for (y in reg.len:data.len)
  {
    sample.vector[1:reg.len] <- data.vector[y - reg.len + (1:reg.len)]
    
    sample.var[y - reg.len + 1] <- var(sample.vector)
  }
  
  data.var <- mean(sample.var)
  
  reg.count <- 1
  reg.start <- 1
  
  for (y in 1:data.len)
  {
    if (abs(rsi.vector[y]) > 0.0)
    {
      reg.count <- reg.count + 1
      reg.end <- y - 1
      n <- reg.end - reg.start + 1
      local.vector <- array(0, dim = n)
      
      local.vector[(reg.start:reg.end) - reg.start + 1] <- data.vector[reg.start:reg.end]
      
      local.mean <- mean(local.vector)
      local.wtmean <- weighted.average(local.vector, data.var)
      
      for (newy in reg.start:(y - 1))
      {
        reg.mean[newy] <- local.mean
        reg.wtmean[newy] <- local.wtmean
        reg.length[newy] <- reg.end - reg.start + 1
      }
      
      reg.start <- y
    }
  }
  
  n <- data.len - reg.start + 1
  local.vector <- array(0, dim = n)
  
  local.vector[(reg.start:data.len) - reg.start + 1] <- data.vector[reg.start:data.len]
  
  local.mean <- mean(local.vector)
  local.wtmean <- weighted.average(local.vector, data.var)
  
  for (newy in reg.start:data.len)
  {
    reg.mean[newy] <- local.mean
    reg.wtmean[newy] <- local.wtmean
    reg.length[newy] <- data.len - reg.start + 1
  }
  
  
  reg.start <- 1
  
  for (y in 2:(data.len - 1))
  {
    if (abs(rsi.vector[y]) > 0.0)
    {
      if ((y - reg.start) > 1)
      {
        reg.t.test <- t.test(data.vector[reg.start:(y - 1)], data.vector[y:(y - 1 + reg.length[y])], alternative = "two.sided", paired = FALSE, var.equal = FALSE)
        reg.ci[y] <- reg.t.test$p.value
      }
      
      reg.start <- y
    }
  }
  
  return(data.frame(cbind(rsi = rsi.vector, mean = reg.mean, weighted.mean = reg.wtmean, length = reg.length, conf = reg.ci)))
}

calculate.rsi <- function()
{
  if (!is.numeric(data.len) || data.len < 2 || !is.numeric(p.level) || p.level <= 0.0 || p.level > 0.5)
  {
    return(-1)
  }
  
  rsi <- array(0.0, dim = data.len)
  
  sample.vector <- array(0.0, dim = reg.len)
  
  sample.var <- array(0.0, dim = (data.len - reg.len + 1))
  
  for (y in reg.len:data.len)
  {
    sample.vector[1:reg.len] <- data.vector[y - reg.len + (1:reg.len)]
    
    sample.var[y - reg.len + 1] <- var(sample.vector)
  }
  
  data.var <- mean(sample.var)
  
  ttest.df <- (2 * reg.len) - 2
  
  reg.diff <- qt((1.0 - (p.level / 2.0)), ttest.df, lower.tail=TRUE) * sqrt(2.0 * data.var / reg.len)
  
  sample.vector[1:reg.len] <- data.vector[1:reg.len]
  
  reg.mean <- weighted.average(sample.vector, data.var)
  
  change.point <- 1
  
  for (y in 2:data.len)
  {
    rsi[y] <- 0.0
    
    if (data.vector[y] > (reg.mean + reg.diff))
    {
      rsi[y] <- cusum.up(y, reg.mean, reg.diff, data.var)
    }
    
    if (data.vector[y] < (reg.mean - reg.diff))
    {
      rsi[y] <- cusum.down(y, reg.mean, reg.diff, data.var)
    }
    
    if (abs(rsi[y]) > 0.0 && y > (data.len - reg.len + 1))
    {
      return(rsi)
    }
    
    if (rsi[y] == 0.0)
    {
      if (y >= (change.point + reg.len))
      {
        n <- y - change.point + 1
        
        sample.vector <- array(0, dim = n)
        
        sample.vector[1:n] <- data.vector[change.point + (1:n) - 1]
        
        reg.mean <- weighted.average(sample.vector, data.var)
        
        ttest.df <- (2 * reg.len) - 2
        
        reg.diff <- qt((1.0 - (p.level / 2.0)), ttest.df, lower.tail=TRUE) * sqrt(2.0 * data.var / reg.len)
      }
    }
    else
    {
      change.point <- y
      
      sample.vector <- array(0, dim = reg.len)
      
      sample.vector[1:reg.len] <- data.vector[change.point + (1:reg.len) - 1]
      
      reg.mean <- weighted.average(sample.vector, data.var)
      
      ttest.df <- (2 * reg.len) - 2
      
      reg.diff <- qt((1.0 - (p.level / 2.0)), ttest.df, lower.tail=TRUE) * sqrt(2.0 * data.var / reg.len)
    }
  }
  
  return(rsi)
}

weighted.average <- function(data.vector, data.var)
{
  if (!is.numeric(data.vector) || length(data.vector) < 2 || !is.numeric(data.var) || data.var <= 0.0)
  {
    return(-1);
  }
  
  nyears <- length(data.vector)
  
  data.avg <- mean(data.vector)
  
  for (i in 1:2)
  {
    sum.weight <- 0
    sum.avg <- 0
    
    for (y in 1:nyears)
    {
      data.dev <- (data.vector[y] - data.avg) / sqrt(data.var)
      
      if (data.dev == 0.0)
      {
        datum.weight <- 1.0
      }
      else
      {
        datum.weight <- min(1.0, (huber.val / abs(data.dev)))
      }
      
      sum.weight <- sum.weight + datum.weight
      sum.avg <- sum.avg + (data.dev * datum.weight)
    }
    
    sum.avg <- (sum.avg / sum.weight)
    sum.avg <- data.avg + (sum.avg * sqrt(data.var))
    data.avg <- sum.avg
  }
  
  return(data.avg)
}

cusum.up <- function(start.year, reg.mean, reg.diff, data.var)
{
  if (!is.numeric(start.year) || start.year < 1)
  {
    return(-1)
  }
  
  cusum <- 0.0
  sum.weight <- 0.0
  
  syr <- as.integer(start.year)
  lreg <- as.integer(reg.len)
  ldat <- as.integer(data.len)
  
  for (y in (syr):(syr + lreg - 1))
  {
    if (y > ldat)
    {
      if (sum.weight > 0.0)
      {
        cusum <- (cusum / sum.weight)
        return(cusum)
      }
    }
    
    data.dev <- (data.vector[y] - reg.mean - reg.diff) / sqrt(data.var)
    
    if (data.dev == 0.0)
    {
      datum.weight <- 1.0
    }
    else
    {
      datum.weight <- min(1.0, (huber.val / abs(data.dev)))
    }
    
    sum.weight <- sum.weight + datum.weight
    cusum <- cusum + (data.dev * datum.weight)
    
    if (cusum < 0.0)
    {
      cusum <- 0.0
      return(cusum)
    }
  }
  
  cusum <- (cusum / sum.weight)
  
  return(cusum)
}


cusum.down <- function(start.year, reg.mean, reg.diff, data.var)
{
  if (!is.numeric(start.year) || start.year < 1)
  {
    return(-1)
  }
  
  cusum <- 0.0
  sum.weight <- 0.0
  
  syr <- as.integer(start.year)
  lreg <- as.integer(reg.len)
  ldat <- as.integer(data.len)
  
  for (y in (syr):(syr + lreg - 1))
  {
    if (y > ldat)
    {
      if (sum.weight > 0.0)
      {
        cusum <- (cusum / sum.weight)
        return(cusum)
      }
    }
    
    data.dev <- (data.vector[y] - reg.mean + reg.diff) / sqrt(data.var)
    
    if (data.dev == 0.0)
    {
      datum.weight <- 1.0
    }
    else
    {
      datum.weight <- min(1.0, (huber.val / abs(data.dev)))
    }
    
    sum.weight <- sum.weight + datum.weight
    cusum <- cusum + (data.dev * datum.weight)
    
    if (cusum > 0.0)
    {
      cusum <- 0.0
      return(cusum)
    }
  }
  
  cusum <- (cusum / sum.weight)
  
  return(cusum)
}


