#Discrete-time Logistic Population Growth Simulations
#August 15, 2014
#By Stephen B. Heard
#Non-commercial use permitted.
# https://scientistseessquirrel.wordpress.com/2016/03/24/teaching-population-dynamics-with-simulations-in-r/


#Set number of generations to plot
PlotGen <- 200

###Set parameters for simulation 1###

# set initial population size
N0 <- 10

#Set R
R <- 2

#Set K
K <- 500


###Run simulation###

# initialize vector to hold results 
PopSize <- N0 

# create variable to hold the current population size
PopNow <- N0 

# calculate population sizes and append to popsize
for(i in 1:PlotGen) { 
  PopNow <- PopNow + PopNow*R*(1-PopNow/K)    #discrete logistic
  if (PopNow < 0) {PopNow <- 0}
  PopSize <- c(PopSize,PopNow)                #add result to vector
}


###Plot results###

tvals <- 1:length(PopSize) #create a vector of time values equal to length of results vector

plot(tvals[1:PlotGen], PopSize[1:PlotGen],type="o",col="red", 
     xlab="Generation",ylab="Population size",pch=16,cex=.75)
