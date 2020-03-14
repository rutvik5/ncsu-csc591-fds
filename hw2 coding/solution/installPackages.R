
# Install necessary packages

#cleanup before start
rm(list=ls(all=T))

packages <- list('data.table', 'caret', 'biglm') #Add all your packages here. Don't change the rest of the file, or file name
#Example: packages <- list('data.table','rpart','rgdal')


InstallAll <- function(packageName){
if(!(packageName %in% installed.packages())){
  install.packages(packageName,dependencies = T)
}
}

sapply(packages, InstallAll)
