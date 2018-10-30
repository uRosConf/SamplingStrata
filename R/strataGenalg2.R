strataGenalg2 <- function (
    errors, 
    frame, 
    ncuts,
    dominio, 
    minnumstr, 
    iter, 
    pops, 
    mut_chance, 
    elitism_rate, 
    suggestions, 
    realAllocation, 
    writeFiles, 
    showPlot
)
{
  mutationChance <- mut_chance
  elitism <- elitism_rate * pops
#--------------------------------
# Functions for Genetic Algorithm
#--------------------------------
evaluate <- function(dataset,errors,string=c(),ncuts) {
  frame <- dataset
  nX <- sum(grepl("X",colnames(frame)))
  for(i in 1:nX){
      eval(parse(text=paste("frame$ZZ",i,"<- frame$X",i,sep="")))
  }
  v<-string
  for(j in 1:nX){
      ini=(j-1)*(NROW(v)/nX)+1
      fin=j*(NROW(v)/nX)
      eval(parse(text=paste("v",j,"<-string[ini:fin]*max(frame$ZZ",i,")",sep="")))
      eval(parse(text=paste("x",j,"_cuts<-as.data.frame(v",j,"[order(v",j,")])",sep="")))
      eval(parse(text=paste("x",j,"_cuts<-as.data.frame(rbind(min(frame$ZZ",j,")",",x",j,"_cuts,max(frame$ZZ",j,")))",sep="")))
      eval(parse(text=paste("x",j,"_cuts$lim<-x",j,"_cuts$`v",j,"[order(v",j,")]`",sep="")))
      eval(parse(text=paste("x",j,"_cuts$`v",j,"[order(v",j,")]`<-NULL",sep="")))
      eval(parse(text=paste("frame$X",j," <- NULL",sep="")))
      }

  for(i in 1:(ncuts+1)) {
    eval(parse(text=paste("frame$c",i,"<-0",sep="")))
        for(j in 1:nX) {
          eval(parse(text=paste("frame$c",i,"<-ifelse((frame$ZZ",j,">=x",j,"_cuts$lim[",i,"] & frame$ZZ",j,"<= x",j,"_cuts$lim[",i+1,"]),",i,",frame$c",i,")",sep="")))
    }  
  }
  frame$X1=apply(frame[,c((ncol(frame)-ncuts):ncol(frame))],1,max)
  strata <- buildStrataDF(frame,progress = FALSE)
  size <- sum(stratContig::bethel(strata,errors))
  size
}

monitor <- function(obj) {
  ylim=c(min(obj$evaluation),max(obj$evaluation))
  plot(obj$mean,type="l",col="red",ylim=ylim,xlab="Iterations",ylab="Sample size")
  points(obj$best,type="l",col="black")
}

#-------------------------------
# Execution of Genetic Algorithm
#-------------------------------
stringMin = rep(0,ncuts*sum(grepl("X",colnames(frame))))
stringMax = rep(1,ncuts*sum(grepl("X",colnames(frame))))
rbga.results = rbga2(
                    frame,
                    errors,
                    ncuts,
                    stringMin, 
                    stringMax, 
                    suggestions,
                    pops,
                    iter,
                    mutationChance,
                    mutationFactor=0.5,
                    elitism,
                    monitorFunc=monitor, 
                    evalFunc=evaluate, 
                    verbose=TRUE)

title(paste("Best solution: ",round(min(rbga.results$best),2)))
# Reconstruction of the optimal solution    
# frame <- buildFrameDF(swissmunicipalities,
#                       id = "id",
#                       domainvalue = "domain",
#                       X = c("Surfacesbois","Surfacescult"),
#                       Y = c("Pop020", "Pop2040")
#                       )
nX <- sum(grepl("X",colnames(frame)))
out <- rbga.results
string <- out$population[which(out$evaluations==min(out$evaluations))[1],]
for(i in 1:nX){
      eval(parse(text=paste("frame$ZZ",i,"<- frame$X",i,sep="")))
    }
v<-string
for(j in 1:nX){
  ini=(j-1)*(NROW(v)/nX)+1
  fin=j*(NROW(v)/nX)
  eval(parse(text=paste("v",j,"<-string[ini:fin]*max(frame$ZZ",i,")",sep="")))
  eval(parse(text=paste("x",j,"_cuts<-as.data.frame(v",j,"[order(v",j,")])",sep="")))
  eval(parse(text=paste("x",j,"_cuts<-as.data.frame(rbind(min(frame$ZZ",j,")",",x",j,"_cuts,max(frame$ZZ",j,")))",sep="")))
  eval(parse(text=paste("x",j,"_cuts$lim<-x",j,"_cuts$`v",j,"[order(v",j,")]`",sep="")))
  eval(parse(text=paste("x",j,"_cuts$`v",j,"[order(v",j,")]`<-NULL",sep="")))
  eval(parse(text=paste("frame$X",j," <- NULL",sep="")))
}
    
for(i in 1:(ncuts+2-1)) {
  eval(parse(text=paste("frame$c",i,"<-0",sep="")))
  for(j in 1:nX) {
    eval(parse(text=paste("frame$c",i,"<-ifelse((frame$ZZ",j,">=x",j,"_cuts$lim[",i,"] & frame$ZZ",j,"<= x",j,"_cuts$lim[",i+1,"]),",i,",frame$c",i,")",sep="")))
  }  
}
frame$X1=apply(frame[,c((ncol(frame)-ncuts):ncol(frame))],1,max)
strata <- buildStrataDF(frame,progress = FALSE)
# size <- sum(bethel(strata,errors))
# size
soluz <- bethel(strata, 
                errors, 
                minnumstr, 
                printa = FALSE,
                realAllocation = realAllocation)
#-----------------------------------------------------  

risulta <- cbind(strata, soluz)
cat("\n *** Sample cost: ", sum(soluz))
cat(paste("\n *** Number of strata: ", nrow(strata)))
# if (writeFiles == TRUE) {
#   sink()
#   # sink(file = fileres, append = TRUE)
#   cat("\n *** Sample cost: ", sum(soluz))
#   cat(paste("\n *** Number of strata: ", nrow(strata)))
#   colnames(risulta) <- toupper(colnames(risulta))
#   # fileout <- file.path(direnew, paste0("outstrata", dominio, ".txt"))
#   # write.table(risulta, file = fileout, sep = "\t", row.names = FALSE, 
#   #             col.names = TRUE, quote = FALSE)
#   write.table(risulta,"outstrata.txt", sep = "\t", row.names = FALSE, 
#               #             col.names = TRUE, quote = FALSE)
#   # cat("\n...written output to", fileout)
#   # sink()
# }
# Preparation of solution list
solution <- list(frame[,c("ID","X1")],risulta,rbga.results)
# solution[[1]] <- frame$X1
# solution[[2]] <- risulta
# solution[[3]] <- rbga.results
return(solution)
}