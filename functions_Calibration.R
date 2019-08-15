Sys.setenv(R_LIBS_USER = 
              '//marral.local/Data/Users/Maria.Xenochristou/Documents/R/R-3.5.1/library')
 
 library("xlsx")
 library("epanet2toolkit", lib.loc = 
           "//marral.local/Data/Users/Maria.Xenochristou/Documents/R/R-3.5.1/library")
 library("GA")
 library("doParallel")

setAllPipeRoughness <- function(variables, ...){
   
   roughnesses <- c(...)
   
   #Get roughness of the pipes
   links <- ENgetcount(2)
   index <- c(1:links)
   
   #Get pipe roughness
   link_roughness <- NULL
   
   for (kk in index){
     link_roughness[[kk]] <- ENgetlinkvalue(kk, 2)
   } 
   
   #Divide links into groups
   roughness_init <- names(variables)
   
   for(i in 1:length(roughness_init)){
     for(ii in variables[[i]]){
       if( ENgetlinktype(ii) < 2){
         
         ENsetlinkvalue(ii, "EN_ROUGHNESS", roughnesses[i])
       }
     }
   }
 }
 
sse <- function(node_orig){
   
   # Get count for nodes, links, tanks
   NoNodes <- ENgetcount("EN_NODECOUNT")
   Index <- c(1:NoNodes)
   
   modeled <- NULL
   node_ID <- NULL
   N <- length(Index)
   error <- rep(NA, N)
   for( i in 1:N){
     modeled[i] <- ENgetnodevalue( i, "EN_PRESSURE")
     node_ID[i] <- ENgetnodeid(i)
   }
   
   node_modeled <- data.frame(ID = node_ID,
                              modeled = modeled)
   node_modeled$Time <- ENgettimeparam("EN_HYDSTEP")/3600
   
   node_final <- merge (node_orig, node_modeled)
   error <- node_final$pressure-node_final$modeled
   
   sse <- sum( error * error )
 }

calibObj <- function(node_orig,variables,  ...){
   
   setAllPipeRoughness(variables, ...)
   ENsolveH()
   sse(node_orig)
 }
 
pipe_groups <- function(){
  
   #Get roughness of the pipes
   links <- ENgetcount(2)
   index <- c(1:links)
   
   #Get pipe roughness
   link_roughness <- NULL
   
   for (kk in index){
       link_roughness[[kk]] <- ENgetlinkvalue(kk, 2)
     } 
     
     #Divide links into groups
     split(index, link_roughness)
     
}

calibObj_24 <- function(node_orig,variables,  ...){
  
  ENopenH()
  setAllPipeRoughness(variables, ...)
  # Get count for nodes, links, tanks
  NoNodes <- ENgetcount("EN_NODECOUNT")
  Index <- c(1:NoNodes)
  
  modeled <- NULL
  node_ID <- NULL
  Time <- NULL
  N <- length(Index)
  error <- rep(NA, N)
  for( i in 1:N){
    node_ID[i] <- ENgetnodeid(i)
  }
  
  t = NULL
  kk = 0
  
  ENinitH(11);
  
  repeat {
    
    t <- c(t, ENrunH())
    
    for( i in 1:N){
      modeled[i+kk] <- ENgetnodevalue( i, "EN_PRESSURE")
      node_ID[i+kk] <- ENgetnodeid(i)
      Time[i+kk] <- ENrunH()/3600
    }
    
    tstep <- ENnextH()
    kk = kk + NoNodes
    
    if (tstep == 0) {
      break
    }
  }
  
  node_modeled <- data.frame(ID = node_ID,
                             modeled = modeled,
                             Time = Time)
  
  ENcloseH()
  
  node_final <- merge (node_orig, node_modeled)
  node_final$error <- abs(node_final$pressure-node_final$modeled)
  
  sum( abs(node_final$error)^2)
}

calibObj_2 <- function(node_orig,variables,  ...){
  
  ENopenH()
  setAllPipeRoughness(variables, ...)

  # Get count for nodes, links, tanks
  t = NULL
  kk = 1
  Tanks_index = NULL
  
  modeled <- data.frame(matrix(0, ncol = (length(Tanks_id)+1), nrow = 2))
  colnames(modeled) <- c( "Time", Tanks_id)
  for (ii in 1:length(Tanks_id)){
    Tanks_index[ii] <- ENgetnodeindex(Tanks_id[ii])
  }
  
  ENinitH(11);
  
  repeat {
    t <- c(t, ENrunH())
    
    for(i in Tanks_index){
      modeled[kk, ENgetnodeid(i)] <- ENgetnodevalue( i, "EN_HEAD")
    }
    
    kk = kk+1
    tstep <- ENnextH()
    if (tstep == 0) {
      break
    }
  }
  
  modeled$Time <- t
  ENcloseH()
  
  node_final <- merge (node_orig, modeled, by = "Time", all = FALSE)
  
  error <- NULL
  
  for (l in 2:13){
    error[l-1] <- sum(apply(node_final[,c(l, l+12)], 1, function(x) abs(diff(x))))
  } 
  
  sum(error^2)
}