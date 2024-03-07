
## Check package instalation
if(!require("Hmisc")) install.packages("Hmisc")

## Load data from the codebook
codebook <- readxl::read_excel("Input/Codebook.xlsx")

## For loop of the labelling process
for(i in colnames(ens)){
  if(i %in% as.vector(unlist(variables))){
    Hmisc::label(ens[[i]]) <- codebook$Variable[codebook$`Name in data` == i]
  }
}

## Free up memory space
rm(codebook); gc()