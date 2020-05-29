# move_files.R
# automate copying of files from ADIOS database to local directory
# obviously only works if have access to ADIOS database

decoder <- read.csv(".\\ADIOS_data\\file_decoder.csv")
nstocks <- length(decoder$Short.Name)

origtext <- "strat_mean_age"
reptext <- "mean_length_at_age"
getfiles <- gsub(origtext, reptext, decoder$ADIOS.name)
getfiles

for (i in 1:nstocks){
  myfile <- read.csv(file = paste0("//net/home0/pdy/pub/STOCKEFF/ADIOS/ADIOS_SV/website/webfiles/", getfiles[i], ".csv"))
  write.csv(myfile, file = paste0(".\\ADIOS_data\\", getfiles[i], ".csv"))
}
