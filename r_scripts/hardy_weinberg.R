## Script written by Corey Chivers, the Bayesian Biologist.
## https://bayesianbiologist.com/2011/06/13/using-simulation-to-demonstrate-theory-hardy-weinberg-equilibrium/

## Script available from
## https://gist.github.com/cjbayesian/468725f4bd7d6f3f027d#file-hw_sim-r


cross<-function(parents)
{
  offspring<-c('d','d') #initiate a child object
  offspring[1]<-sample(parents[1,],1)
  offspring[2]<-sample(parents[2,],1)
  
  return(offspring)
}

random_mating<-function()
{
  tmp_pop<-pop
  for(n in 1:N)
  {
    parents<-sample(1:N,2)
    tmp_pop[n,]<-cross(pop[parents,])
  }
  pop<-tmp_pop
}





genotypes<-c('A','a')

p<-0.5
q<-1-p
N=200
a_freq<-c(p,q)

pop<-array(sample(genotypes,2*N,p=a_freq,replace=T),dim=c(N,2))

I<-200
num_generations=1
g_freq<-array(dim=c(I,3))
p_vec<-array(dim=I)
for(i in 1:I)
{
  p<-runif(1,0,1)
  q<-1-p
  a_freq<-c(p,q)
  
  pop[,1]<-sample(genotypes,N,p=a_freq,replace=T)
  pop[,2]<-sample(genotypes,N,p=a_freq,replace=T)
  
  for(g in 1:num_generations)
    random_mating()
  
  
  f_aa<-0
  f_Aa<-0
  f_AA<-0
  
  for(n in 1:N)
  {
    if(identical(pop[n,],c('A','A')))
      f_AA=f_AA+1
    if(identical(pop[n,],c('A','a')) || identical(pop[n,],c('a','A') ))
      f_Aa=f_Aa+1
    if(identical(pop[n,],c('a','a')))
      f_aa=f_aa+1
    
  }
  f_aa<-f_aa/N
  f_Aa<-f_Aa/N
  f_AA<-f_AA/N
  
  g_freq[i,]<-c(f_AA,f_Aa,f_aa)
  p_vec[i]<-p	
}
#pdf('HW.pdf')
## Plot the sims
plot(p_vec,g_freq[,1],col='forestgreen',xlab='p, or 1-q',ylab='Gentoype Frequency', las=1)
points(p_vec,g_freq[,2],col='darkorange',pch=2)
points(p_vec,g_freq[,3],col='purple', pch=5)


## Theoretical Curves
curve(x^2,col='forestgreen',add=T)
curve(2*x*(1-x),col='darkorange',add=T)
curve((1-x)^2,col='purple',add=T)
#dev.off()
