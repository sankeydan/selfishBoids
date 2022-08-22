quickLibrary = function ( fold.path){
  files = list.files ( fold.path)
  for  ( i in 1:length(files)){
  source  ( file.path ( fold.path, files[i]))
  }
}
