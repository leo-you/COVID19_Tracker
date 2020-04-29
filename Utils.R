sourceDirectory <- function(path){
  if(!dir.path(path)){
    warning(paste(path," is not a valid path!"))
    return(NULL)
  }
  
  env <- parent.frame()
  files <- list.files(path = path,pattern = ".*\\.R",all.files = FALSE,full.names = TRUE,
                      recursive = FALSE)
  for (aFile in files) {
    tryCatch(
      {
        source(aFile,local = env)
        cat(aFile," is sourced!")
      },
      error = function(cond){
        message("Failed loading the following file \" ",aFile,"\" .")
        message(cond )
      }
    )
  }
}