#Create a skeletonized network in the same folder
setwd("C:/.../")

system("spotSkeleton NameofInputFile.inp 300 NameofSkeletonizedFile.inp")

#Take calibration data from the original network############
# Open the EPANET toolkit & hydraulics solver
ENopen("NameofInputFile.inp", "NameofInputFile.rpt", "")
ENopenH()
ENsettimeparam("EN_HYDSTEP", 3600)
ENsettimeparam("EN_QUALSTEP", 3600)
ENsettimeparam("EN_PATTERNSTEP", 3600)
ENsettimeparam("EN_REPORTSTEP", 3600)
ENsettimeparam("EN_DURATION", 7200)

#Get Indexes
node_ID <- NULL

# Get count for nodes, links, tanks
NoNodes <- ENgetcount("EN_NODECOUNT")

for (i in 1:NoNodes){
  node_ID[i] <- ENgetnodeid(i)
}

Tanks_id <- node_ID[(NoNodes-11):NoNodes]
Tanks_index <- c((NoNodes-11):NoNodes)

node_pressure <- data.frame(matrix(0, ncol = (length(Tanks_id)+1), nrow = 1))
colnames(node_pressure) <- c( "Time", Tanks_id)

t = NULL
kk = 1

ENinitH(11);

repeat {
  t <- c(t, ENrunH())
  
  for(i in Tanks_index){
    node_pressure[kk, ENgetnodeid(i)] <- ENgetnodevalue( i, "EN_HEAD")
  }
  
  kk = kk+1
  tstep <- ENnextH()

    if (tstep == 0) {
    break
  }
}

node_pressure$Time <- t
node_orig <- node_pressure

ENcloseH()
ENclose()
###########

#Open the skeletonized network you want to calibrate#####
ENopen("NameofSkeletonizedFile.inp", "NameofSkeletonizedFile.rpt", "")

ENsettimeparam("EN_HYDSTEP", 3600)
ENsettimeparam("EN_QUALSTEP", 3600)
ENsettimeparam("EN_PATTERNSTEP", 3600)
ENsettimeparam("EN_REPORTSTEP", 3600)
ENsettimeparam("EN_DURATION", 7200)

#Choose default
variables <- pipe_groups()

#Run your GA####
Nogroups <- length(variables)
x <- rep(120, Nogroups)

GA <- ga(type="real-valued", fitness = function(x) -calibObj_2(node_orig, variables, x),
         lower=rep(20, Nogroups), upper=rep(200, Nogroups), popSize= 500, maxiter = 3,
         keepBest = TRUE)

#Save an input file with the new roughness coefficients
ENsaveinpfile("//.../CalibratedNetwork.inp")

ENcloseH()
ENclose()

#Plot summary of the results
summary(GA)
plot(GA)
#######
