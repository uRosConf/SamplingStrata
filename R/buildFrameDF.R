buildFrameDF <- function(df,id,X,Y,domainvalue) {
  vars <- colnames(df)
  if (!id %in% vars) stop("Id name not in the frame variables")
  # if (!X %in% vars) stop("X names not in the frame variables")
  for (i in (1:length(X))) {
    var = X[i]
    if (!var %in% vars) {
      msg = paste("X variable ",var," not in the frame variables",sep="")
      stop(msg)
    }
  }
  for (i in (1:length(Y))) {
    var = Y[i]
    if (!var %in% vars) {
       msg = paste("Y variable ",var," not in the frame variables",sep="")
       stop(msg)
    }
  }
  if (any(!domainvalue %in% vars)){
    stop("Not all domain variables are present in the frame variables")  
  } 
  dframe <- NULL
  stmt <- paste("dframe$id <- df$",as.character(id),sep="")
  eval(parse(text=stmt))
  for (i in (1:length(X))) {
    aux <- paste("dframe$X",i," <- df$",as.character(X[i]),sep="")
    eval(parse(text=aux))
  }
  for (i in (1:length(Y))) {
    target <- paste("dframe$Y",i," <- df$",as.character(Y[i]),sep="")
    eval(parse(text=target))
  }
  for (i in (1:length(domainvalue))) {
    stmt <- paste0("dframe$domainvalue",i," <- df$",as.character(domainvalue[i]),sep="")
    eval(parse(text=stmt))
  }
  
  dframe <- as.data.frame(dframe)
  return(dframe)
}