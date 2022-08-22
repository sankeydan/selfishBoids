fold = file.path   ( getwd()  , "R")
files = list.files(fold)
for(i in 1:length(files)){
  source( file.path(fold, files[i]))
}
