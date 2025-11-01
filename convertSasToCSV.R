# First check if the ovariance csv file exists
if (file.exists("ovarian.csv")) {
  # If it exists, quickly read it in
  Ovarian <- data.table::fread("ovarian.csv")
} else {
  # If the ovariance file doesnt exist, load in the sas file using the 
  # sas7bdat package, then covert it to csv (for easier loading next time)
  library(sas7bdat)
  Ovarian <- read.sas7bdat("ovcasurvival_083022.sas7bdat")
  write.csv(Ovarian, "ovarian.csv")
}