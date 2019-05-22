expected_CV <- function (strata, newframe = NULL) {
  M_h <- S_h <- NULL
  if (is.null(strata$SOLUZ)) stop("There is no allocation of units in strata")
  ndom <- length(unique(strata$DOM1))
  nvars <- (ncol(strata) - 6) / 2
  cv <- matrix(NA,nrow=ndom,ncol=nvars)
  colnames(cv) <- paste("cv(Y",c(1:nvars),")",sep="")
  rownames(cv) <- paste("DOMAIN1_value:",c(1:ndom),sep="")
  k<-0
  for (i in (as.numeric(levels(as.factor(strata$DOM1))))) {
    k<-k+1
    stratadom <- strata[strata$DOM1 == i,]
    for (j in 1:nvars) {
      n_h <- stratadom$SOLUZ
      N_h <- stratadom$N
      stmt <- paste("S_h <- stratadom$S",j,sep="")
      eval(parse(text=stmt))
      stmt <- paste("M_h <- stratadom$M",j,sep="")
      eval(parse(text=stmt))
      Y_h <- N_h * M_h
      Var_h <- (N_h^2) * (1 - n_h/N_h) * ((S_h^2)/n_h)
      CV <- sqrt(sum(Var_h)) / sum(Y_h)
      cv[k,j] <- CV 
    }
  }
  cv <- round(cv,3)
  if(is.null(newframe)){
    return(cv)
  }else{
    domainVariables <- colnames(newframe)[substring(colnames(newframe),1,11)=="DOMAINVALUE"]
    if(length(domainVariables)==1){
      return(cv)
    }
    newDomainVariables <- domainVariables[domainVariables != "DOMAINVALUE1"]
    yVariables <- colnames(newframe)[substring(colnames(newframe),1,1)=="Y"]
    cvNew <- list()
    strataNew <- list()
    for(idom in seq_along(newDomainVariables)+1){
      unVal <- unique(newframe[[paste0("DOMAINVALUE",idom)]])
      cvNew[[idom-1]] <- matrix(NA,nrow=length(unVal),ncol=length(yVariables))
      colnames(cvNew[[idom-1]]) <- paste("cv(Y",c(1:length(yVariables)),")",sep="")
      rownames(cvNew[[idom-1]]) <- paste("DOMAIN",idom,"_value:",c(1:length(unVal)),sep="")
      for(iy in seq_along(yVariables)){
        m <- aggregate(newframe[[paste0("Y",iy)]], newframe[,c("LABEL","DOMAINVALUE1",paste0("DOMAINVALUE",idom))],mean)
        s <- aggregate(newframe[[paste0("Y",iy)]], newframe[,c("LABEL","DOMAINVALUE1",paste0("DOMAINVALUE",idom))],sd)
        N <- aggregate(newframe[[paste0("Y",iy)]], newframe[,c("LABEL","DOMAINVALUE1",paste0("DOMAINVALUE",idom))],length)
        colnames(m) <- c("STRATO","DOM1","DOM2","M")
        colnames(s) <- c("STRATO","DOM1","DOM2","S")
        colnames(N) <- c("STRATO","DOM1","DOM2","Nd")
        strataNew[[idom - 1]] <- merge(strata[,c("STRATO","DOM1","SOLUZ","N")],merge(merge(m, s),N))
        strataNew[[idom - 1]]$SOLUZ2 <- round(strataNew[[idom - 1]]$SOLUZ*strataNew[[idom - 1]]$Nd/strataNew[[idom - 1]]$N)
        
        for (ival in seq_along(unVal)) {
          TF <- strataNew[[idom - 1]]$DOM2 == ival
          n_h <- strataNew[[idom - 1]]$SOLUZ2[TF]
          n_h[n_h==0] <- NA
          N_h <- strataNew[[idom - 1]]$Nd[TF]
          S_h <- strataNew[[idom - 1]]$S[TF]
          M_h <- strataNew[[idom - 1]]$M[TF]
          Y_h <- N_h * M_h
          Var_h <- (N_h^2) * (1 - n_h/N_h) * ((S_h^2)/n_h)
          CV <- sqrt(sum(Var_h, na.rm = TRUE)) / sum(Y_h, na.rm=TRUE)
          cvNew[[idom-1]][ival,iy] <- CV 
        }
        
      }
    }
    return(rbind(cv,do.call("rbind",cvNew)))
  }
  
}