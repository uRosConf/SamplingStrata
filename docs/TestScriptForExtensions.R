############################
# SamplingStrata functions #
############################

library(SamplingStrata)

#------------------------------------------------------------------------
# Get data on Swiss municipalities
data(swissmunicipalities)


# Only first two regions
swissmunicipalities <- swissmunicipalities[swissmunicipalities$REG < 3,]
# add a unique identifier
swissmunicipalities$id <- c(1:nrow(swissmunicipalities))


#------------------------------------------------------------------------

# Build sampling frame
swissmunicipalities$sizeclass <- as.numeric(cut(
  swissmunicipalities$Alp, breaks = c(-Inf,0,Inf)
  ))
table(swissmunicipalities$sizeclass)
swissframe <- buildFrameDF(df = swissmunicipalities,
                           id = "id",
                           X = c("Surfacesbois",
                                 "Surfacescult"),
                           Y = c("Pop020",
                                 "Pop2040"),
                           domainvalue = c("REG","sizeclass"))


# Categorize the two stratification variables
swissframe$X1 <- var.bin(swissframe$X1, bins=15)
swissframe$X2 <- var.bin(swissframe$X2, bins=15)
#swissframe$DOM0 <- 1
table(swissframe$X1)
table(swissframe$X2)

summary(swissframe)

#------------------------------------------------------------------------

# Build atomic strata
swissstrata <- buildStrataDF(swissframe, progress = TRUE)

head(swissstrata)

# Define precision constraints

swisserrors <- as.data.frame(list(DOM=c(rep("DOM1",3)),
                                  CV1=c(0.08,0.10,.1),
                                  CV2=c(0.12,0.15,15),
                                  domainvalue=c(1:3)
                            )
                            )

# check input data

checkInput(errors = swisserrors[-1,], 
           strata = swissstrata, 
           sampframe = swissframe)


#########################################################################

# Optimization I (on categorical stratification variables)

set.seed(123)
solution1 <- optimizeStrata(
  errors = swisserrors[-1,], 
  strata = swissstrata,
  iter = 50,
  pops = 10,
  writeFiles = TRUE,
  showPlot = TRUE,
  parallel = FALSE)
sum(ceiling(solution1$aggr_strata$SOLUZ))
expected_CV(solution1$aggr_strata)


# update the frame and the strata

newstrata <- updateStrata(swissstrata, 
                          solution1, 
                          writeFiles = TRUE)

framenew <- updateFrame(swissframe, 
                        newstrata, 
                        writeFiles=FALSE)

expected_CV(solution1$aggr_strata, framenew)
# select the sample
sample <- selectSample(framenew, 
                       solution1$aggr_strata, 
                       writeFiles = FALSE)
