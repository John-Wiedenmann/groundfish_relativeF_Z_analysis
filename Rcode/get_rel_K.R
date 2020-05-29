# get_rel_K.R
# pull relative condition factor (K) from ADIOS, summarize, and save in ADIOS_dat dir
# raw files are too big to move, so have to save only the summary info

library("dplyr")

decoder <- read.csv(".\\ADIOS_data\\file_decoder.csv")
nstocks <- length(decoder$Short.Name)

origtext <- "strat_mean_age"
reptext <- "relative_k"
getfiles <- gsub(origtext, reptext, decoder$ADIOS.name)

for (i in 1:nstocks){
  dat <- read.csv(file = paste0("//net/home0/pdy/pub/STOCKEFF/ADIOS/ADIOS_SV/website/webfiles/", getfiles[i], ".csv"))
  
  sdat <- dat %>%
    group_by(SURVEY, YEAR) %>%
    summarize(samp_size = n(),
              medianK = median(K_rel), 
              P25 = quantile(K_rel, probs = 0.25), 
              P75 = quantile(K_rel, probs = 0.75))
  
  sdat$stock <- decoder$Short.Name[i]
  write.csv(sdat, file = paste0(".\\ADIOS_data\\", getfiles[i], "_summarized.csv"))
}
