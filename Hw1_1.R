# create a simlist, which will be the return value, an R environment
newsim <- function(dbg=F) {
  simlist <- new.env()
  simlist$currtime <- 0.0  # current simulated time
  simlist$evnts <- NULL  # event list
  simlist$dbg <- dbg
  simlist
}

# insert event evnt into evnts in simlist
insevnt <- function(evnt,simlist) {
  # if the event list is empty, set it to consist of evnt and return
  if (is.null(simlist$evnts)) {
    simlist$evnts <- matrix(evnt,nrow=1)
    return()
  }
  # otherwise, find insertion point
  inspt <- binsearch(simlist$evnts[,1],evnt[1])
  # now "insert," by reconstructing the matrix; we find what portion of
  # the current matrix should come before evnt and what portion should 
  # come after it, then string everything together
  before <- if (inspt == 1) NULL else simlist$evnts[1:(inspt-1),]
  nr <- nrow(simlist$evnts)
  after <- if (inspt <= nr) simlist$evnts[inspt:nr,] else NULL  
  simlist$evnts <- rbind(before,evnt,after)  
  rownames(simlist$evnts) <- NULL
}

# schedule new event in evnts in simlist; evnttime is the time at
# which the event is to occur; evnttype is the event type; appdata is
# a vector of numerical application-specific data
schedevnt <- function(evnttime,evnttype,simlist,appdata=NULL) {
  evnt <- c(evnttime,evnttype,appdata)
  insevnt(evnt,simlist)  
}

# start to process next event (second half done by application
# programmer via call to reactevnt() from mainloop())
getnextevnt <- function(simlist) {
  head <- simlist$evnts[1,]
  # delete head
  if (nrow(simlist$evnts) == 1) simlist$evnts <- NULL else 
    simlist$evnts <- simlist$evnts[-1,,drop=F]  
  return(head)
}

# main loop of the simulation
mainloop <- function(simlist,simtimelim) {
  while(simlist$currtime < simtimelim) {
    head <- getnextevnt(simlist)  
    # update current simulated time
    simlist$currtime <- head[1]  
    # process this event (programmer-supplied ftn)
    simlist$reactevent(head,simlist)  
    if (simlist$dbg) {
      print("event occurred:")
      print(head)
      print("events list now")
      print(simlist$evnts)
      browser()
    }
  }
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; this could be replaced
# by faster C code
binsearch <- function(x,y) {
  n <- length(x)
  lo <- 1
  hi <- n
  while(lo+1 < hi) {
    mid <- floor((lo+hi)/2)
    if (y == x[mid]) return(mid)
    if (y < x[mid]) hi <- mid else lo <- mid
  }
  if (y <= x[lo]) return(lo)
  if (y < x[hi]) return(hi)
  return(hi+1)
}

# appendtofcfsqueue() and delfcfsqueuehead() below assume the
# application code has one or more queues, each queue stored as a
# list-of-lists, with each individual list being the information for one
# queued job; note that one must initialize the list-of-lists as NULL 

# appends jobtoqueue to the given queue, assumed of the above form;
# the new, longer list is returned
appendtofcfsqueue <- function(queue,jobtoqueue) {
  lng <- length(queue)
  queue[[lng+1]] <- jobtoqueue
  queue
}

# deletes head of queue; assumes list-of-lists structure as decribed
# above; returns the head and new queue
delfcfsqueuehead <- function(queue) {
  qhead <- queue[[1]]
  newqueue <- queue[-1]
  # careful!--an empty list is not NULL  
  if (length(queue) == 1) newqueue <- NULL
  list(qhead=qhead,newqueue=newqueue)
}


#Author: Yuqi YANG
#Our simulate machine system
machineSim <- function(meanbroken,meanrepair,timelim,dbg=F) {
  #k is the number of machine in this system
  k <- 2 
  numrepairmen <- 1
  #simtimelim is the total time of this simulation
  simtimelim <- timelim
  
  simlist <- newsim(dbg)
  simlist$reactevent <- machinereact  # defined below
  simlist$brokenrate <- 1 / meanbroken #the rate for one machine going broken
  simlist$repairrate <- 1 / meanrepair #the rate for one machine to be repaired
  simlist$result <- c(rep(0,k+1)) #save the total time for each state. e.g. result[1] means the time of state 0 lasted in this system  
  simlist$currtime <- 0 #the current time
  simlist$lastChangeStateTime <- 0 #as its name it is uesd to calculate the time for each state
  simlist$numofmachinework<-k #the number of machine which is active. initial value is k
  simlist$numofrepairman<- numrepairmen #the number of active repairman in the system
  simlist$queue <- NULL #the witlist for machine which need to be repaired but is lack of repairman
  
  #initialize part, there are k machines working in the system
  i<-1
  while (i <= k)
  {
    timetogobroken <- rexp(1,simlist$brokenrate)#for the exponential random variable
    currrntmachineID <- i #just define, non-useful
    schedevnt(timetogobroken,1,simlist,c(currrntmachineID)) 
    i <- i+1
  }
  
  while(simlist$currtime < simtimelim) {   
    head <- getnextevnt(simlist)    
    #update current simulated time, !!!the event in simlist is in the order of which top(head) is latest event
    simlist$currtime <- head[1]  
    # process this event (programmer-supplied ftn)
    
    index <- simlist$numofmachinework+1 #get the index of this state in the result vector 
    time <- simlist$currtime- simlist$lastChangeStateTime #calculate the time for this state lastes
    
    temp<-simlist$result[index]+time
    simlist$result[index]<-temp #add the time to result vector for this state
    
    simlist$lastChangeStateTime <- simlist$currtime # update the lastChangeStateTime   
    
    simlist$reactevent(head,simlist)  
  } 
  
  print(simlist$result/simtimelim)  
  #just for test when k=2,meanbroken=20,meanrepair=8
  print(simlist$result[2]/simlist$result[1]) #state(1)/state(0)
  print(simlist$result[3]/simlist$result[2]) #state(2)/state(1)
}

machinereact <- function(evnt,simlist) {
  etype <- evnt[2]
  if (etype == 1) # machine broken
  {  
    # schedule the time for this machine to go back to work 
    timeofgobackwork <- simlist$currtime + rexp(1,simlist$repairrate)
    machineID<-evnt[3]
    
    #change the state of the system 
    temp <- simlist$numofmachinework-1
    simlist$numofmachinework <- temp
    
    if (simlist$numofrepairman>0)# start repairment
    {
        #schedvnt when this broken machine has been repaired
        schedevnt(timeofgobackwork,2,simlist,c(machineID))
        #cost one repairmen to fix machine
        simlist$numofrepairman<-simlist$numofrepairman-1
    }
    else# no enough repairman add this job to waitlist queue
    {
      simlist$queue <- appendtofcfsqueue(simlist$queue,evnt)
    }
    
  } 
  else if (etype == 2)#this broken machine has been repaired
  {  
      # schedule the time for this machine to break down 
      timeofgobroken <- rexp(1,simlist$brokenrate)
      machineID<-evnt[3]
      
      #change the state of the system
      temp <- simlist$numofmachinework+1
      simlist$numofmachinework <- temp
      
      #finish job, free one repairman
      simlist$numofrepairman<-simlist$numofrepairman+1
      
      #schedvnt when this broken machine has broken down
      schedevnt(simlist$currtime+timeofgobroken,1,simlist,c(machineID))
      if (!is.null(simlist$queue)) {
        #got the first job from waitlist(queue)
        tmp <- delfcfsqueuehead(simlist$queue)
        job <- tmp$qhead
        simlist$queue <- tmp$newqueue
        
        # start job of repairment
        timeofgobackwork <- simlist$currtime + rexp(1,simlist$repairrate)
        machineID<-evnt[3]
        schedevnt(timeofgobackwork,2,simlist,c(machineID))
        
        #cost one repairman
        simlist$numofrepairman<-simlist$numofrepairman-1
      }  
  }
} 