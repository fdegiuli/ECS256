source('DES.R')

#u: mean time to failure of each machine
#r: mean time to repair a failed machine 
#k: number of machines
#simtime: the amount of time to run the simulation for 
#dbg: enable debugging
p1bSim <- function(u, r, k, simtime, dbg=F){
	#create the simulation and setup our application specific data
	simulation <- newsim(dbg) #create a new simulation
	simulation$reactevent <- p1bSimReact #set the react event for the simulation
	simulation$failRate <- 1 / u #set failure rate for the machines
	simulation$repairRate <- 1 / r #set repair rate for each machine
	simulation$numMachinesUp <- k #how many machines are currently running. start with all machines functioning
	simulation$timeOfLastEvent <- 0 #the time the previous event occurred at
	simulation$timeInState <- rep(0, k+1) #amount of time spent in each state. we will use these to estimate the stationary probablities (PIs)
																	#ie times[1] is the amount time with 0 machines running. 
																	#times[2] is the amount of time with 1 machine running and so on
																	
																	
	#schedule the first failure times for each machine
	firstFailTimes <- rexp(k, simulation$failRate) #generate the first failure times for each machine
	#event Ids
	#1 = machine failure
	#2 = machine repaired
	for(machine in 1:k){ #schedule the first fail times for each machine
		schedevnt(firstFailTimes[machine], 1, simulation, machine)
	}
	
	#start the simulation
	mainloop(simulation, simtime)
	
	#update the time spent in the final state during the last part of the simulation
	simulation$timeInState[simulation$numMachinesUp + 1] <- simulation$timeInState[simulation$numMachinesUp + 1] + (simulation$currtime - simulation$timeOfLastEvent)

	percentTimeInState = simulation$timeInState / sum(simulation$timeInState) #the percentage of time spent in each state. Thi
																																						#this is the pi vector from the book
	averageNumWorkingMachines =  sum(percentTimeInState * 0:k) #calculate the expected value for the number of machines operating
	return(averageNumWorkingMachines)
}#p1bSim


p1bSimReact <- function(event, simulation){

	eventType <- event[2] #get this event's type
												#1 = machine failure
												#2 = machine repaired
	
	#update the amount of time spent with this many machines up
	# + 1 in the indexing because R uses 1 based indexing
	simulation$timeInState[simulation$numMachinesUp + 1] <- simulation$timeInState[simulation$numMachinesUp + 1] + (simulation$currtime - simulation$timeOfLastEvent)
	simulation$timeOfLastEvent <- simulation$currtime #update the time when the last event happened
	machineNumber <- event[3] #find out which machine failed or was repaired
	
	if(eventType == 1){ #a machine went down	
		repaired <- simulation$currtime + rexp(1, simulation$repairRate) #generate the time when this machine will be completely repaired
		schedevnt(repaired, 2, simulation, machineNumber) #schedule this machine to be repaired
		simulation$numMachinesUp = simulation$numMachinesUp - 1 #one less working machine :(
		
	} else if( eventType == 2){ #a machine was repaired
		nextBreakDownTime = simulation$currtime + rexp(1, simulation$failRate) #generate the next time for this machine to fail
		schedevnt(nextBreakDownTime, 1, simulation, machineNumber) #schedule the failure
		simulation$numMachinesUp = simulation$numMachinesUp + 1 #one more working machine :)
	}

}#p1bSimReact

#u: mean time to failure of each machine
#r: mean time to repair a failed machine 
#k: number of machines
p1bFindPi <- function(u, r, k){
	failRate <- 1 / u
	repairRate <- 1 / r
	
	Q <- matrix(0, nrow = k+1, ncol = k+1) #k +1 to take into account that there are states from 0 to k
	
	#Lambda[i,j] <- -1 * ((k - i - 1) * failRate + (i - 1) * repairRate) # subtracting 1 because of the 1 based indexing
	for( i in 2:k){
		for(j in (i-1):(i+1)){
			lambda_j <- (k - j - 1) * failRate + (j - 1) * repairRate # subtracting 1 because of the 1 based indexing
			if( i == j){ #on the diagonal
				 Q <- -1 * lambda_j
			} else if ( j < i){ #a machine is going down
				
			} else { # a machine has been repaired
			
			}
		}
	}
	
	

}#p1bFindPi


