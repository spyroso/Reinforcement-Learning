# STAT 497 - Assignment 1
# Winter 2019
# Authors: 
# Bakr Abbas, Matteo Esposito, Spyros Orfanos, Frederic Siino

# Question 1 ------------------------------------------
# (a)
#The k-Armed Bandit: Epsilon Greedy Method
kArmedTestbed = function(k, nplays, epsilon, theseed){
  set.seed(theseed)      #taking the user defined seed
  uniform_rvs = runif(nplays, 0, 1) #simulating uniform rvs for each play
  muvec = rnorm(k)    #true (simulated) value of means for each of the k arms, i.e., q*(a) 
  Rewardvec = rep(0,nplays)   #we will fill this vector with the realized rewards after each play
  Actionvec = rep(0,nplays)   #we will fill this vector with the action taken at each play
  Qmat = matrix(data = 0, nrow = (nplays +1), ncol = k) #This is our approximation of q*(a), and is initially set to 0. 
  
  #e-greedy approach
  for (i in 1:nplays){
    if (uniform_rvs[i] < 1 - epsilon){        
      Actionvec[i] = which.max(Qmat[i,]) #EXPLOIT by choosing the arm that currently has the highest average rewards
    } else {
      Actionvec[i] = sample(k,1)  #EXPLORE by randomly choosing arm
    }
    Rewardvec[i] = rnorm(1, mean = muvec[Actionvec[i]], sd = 1) #simulate reward given by arm selected
    #Update Qmat
    Qmat[i+1, Actionvec[i]] = Qmat[i, Actionvec[i]] + (Rewardvec[i] - Qmat[i, Actionvec[i]]) / length(which(Actionvec == Actionvec[i]))
    Qmat[i+1, -Actionvec[i]] = Qmat[i, -Actionvec[i]]
  }
  Result = list(muvec, Rewardvec, Actionvec) 
  return(Result)
} 

#(b)
n_steps = 1000 #number of steps 
M = 2000 #number of simulations 
epsilon = c(0.1, 0.01, 0)

a = NULL
best_arm = rep(NA,n_steps) 
percent_correct = matrix(data = NA, nrow = n_steps, ncol = M, byrow = TRUE)
correct_chosen = matrix(data = NA, nrow = n_steps, ncol = M, byrow = TRUE)
rewards = matrix(data = NA, nrow = n_steps, ncol = M, byrow = TRUE)
PercOptimalAllepsi = matrix(data = NA, nrow = n_steps, ncol = 3, byrow = TRUE)
AvgRewardAllepsi = matrix(data = NA, nrow = n_steps, ncol = 3, byrow = TRUE)

for (k in 1:3){
  for (j in 1:M){
    a[[j]] = kArmedTestbed(10,n_steps, epsilon[k], 497 + 14*j) #H is the 14th letter!
    best_arm[j] = which.max(a[[j]][[1]])
    for (i in 1:n_steps){
      rewards[i,j] = a[[j]][[2]][i]  #The rewards after each play, for each run
      correct_chosen[i,j] = (a[[j]][[3]][i] == best_arm[j])  #Was the correct arm chosen on the j-th play? A vector for each run.
      percent_correct[i,j] = sum(correct_chosen[1:i,j]) / i *100 #Percent chosen correctly for after each play, for each run
    }
  }
  PercOptimalAllepsi[,k] = rowMeans(percent_correct) #Average across all runs
  AvgRewardAllepsi[,k] = rowMeans(rewards) #Average across all runs
}
AvgRewardAllepsi = t(AvgRewardAllepsi)  
PercOptimalAllepsi = t(PercOptimalAllepsi)

#(c)
plot(x = c(1:1000), AvgRewardAllepsi[1,], type = "l", col = "blue", xlab = "Steps", ylab = "Average Reward", xlim = c(1,1000), axes=FALSE, main = "10 Armed Bandit")
lines(AvgRewardAllepsi[2,], type = "l", col = "red")
lines(AvgRewardAllepsi[3,], type = "l", col = "green4")
legend("bottomright", legend=c("e = 0.1", "e = 0.01","e = 0"),col=c("blue","red","green4"), lty=1:1, cex=1)
axis(2,at=seq(0,2,0.5),labels=T)
axis(1,at=c(1,250,500,750,1000),labels=T)

plot(x = c(1:1000), PercOptimalAllepsi[1,], type = "l", col = "blue", xlab = "Steps", ylab = "% Optimal Action", ylim= c(1,100), axes=FALSE, main = "10 Armed Bandit")
lines(PercOptimalAllepsi[2,], type = "l", col = "red")
lines(PercOptimalAllepsi[3,], type = "l", col = "green4")
legend("bottomright", legend=c("e = 0.1", "e = 0.01","e = 0"),col=c("blue","red","green4"), lty=1:1, cex=1)
axis(2,at=seq(0,100,20),labels=T)
axis(1,at=c(1,250,500,750,1000),labels=T)


# Question 2 ------------------------------------------
#(a)
NonStationaryTestbed = function(isCstStep, alphaparam){
  # Setting constant values for future applications
  epsilon = 0.1
  nplays = 1000 #number of plays 
  k = 2 #number of arms
  
  # Setting up the matrix of simulated means of arms 1 and 2 for 1000 iterations
  set.seed(497)
  z = rnorm(2000)
  z = cbind(z[1:1000],z[1001:2000])
  mumat = matrix(NA, nrow = nplays, ncol = k)
  mumat[1,] = 0 
  for (j in 1:k){
    for (i in 2:nplays){
      mumat[i,j] = mumat[i-1,j] + 0.01*z[i,j] #Autoregressive process defined in assignment instructions
    }
  }
  Rewardvec = rep(NA,nplays)   #we will fill this vector with the realized rewards after each play
  Actionvec = rep(NA,nplays)   #we will fill this vector with the action taken at each play
  Qmat = matrix(data = NA, nrow = 1001, ncol = 2) #This is our approximation of q*(a) at each play
  Qmat[1,] = 0 #Our initial approximations are set to 0
  
  uniform_rvs = runif(nplays, 0, 1) #simulating uniform rvs for each play
  
  # e-greedy approach to chosing which arm to use
  for (i in 1:nplays){
    if (uniform_rvs[i] < 1 - epsilon){    
      Actionvec[i] = which.max(Qmat[i, ]) #EXPLOIT by choosing the arm with highest Q
    } else {
      Actionvec[i] = sample(k,1)  #EXPLORE by randomly choosing arm 
    }
    Rewardvec[i] = rnorm(1, mean = mumat[i, Actionvec[i]], sd = 1) #simulate reward given by arm selected
    
    #Updating the Q-matrix based on if we are using a constant step size or not.
    if (isCstStep == TRUE){
      #Exponentially weighted step size
      Qmat[i+1, Actionvec[i]] = alphaparam*Rewardvec[i] + (1-alphaparam)*Qmat[i, Actionvec[i]] # Recursive method (chosen arm)
      Qmat[i+1, -Actionvec[i]] = Qmat[i, -Actionvec[i]] # Recursive method (NON-chosen arm)
    } else {
      #Equally weighted step size
      Qmat[i+1, Actionvec[i]] = Qmat[i, Actionvec[i]] + (Rewardvec[i] - Qmat[i, Actionvec[i]]) / length(which(Actionvec == Actionvec[i]))
      Qmat[i+1, -Actionvec[i]] = Qmat[i, -Actionvec[i]]
    }
  }
  Result = list(mumat, Rewardvec, Actionvec, Qmat) 
  return(Result)
}  

# (b)
run_1 = NonStationaryTestbed(TRUE, 0.025) #Run using Exponentially Weighted step size, alpha = 0.025
Qmat_r1 = run_1[[4]]

run_2 = NonStationaryTestbed(FALSE)  #Run using Equally Weighted step size
Qmat_r2 = run_2[[4]]

mumat = run_1[[1]]

#Arm 1
plot(c(0:1000), Qmat_r1[,1], type = "l", col = "red", ylim = c(-1,1), xlab = "Stage", ylab = "Reward", main = "Arm 1", xlim = c(0,1000))
lines(Qmat_r2[,1], type = "l", col = "blue")
lines(mumat[,1], type = "l", col = "green4")
legend("topright", legend=c("True", "Exponentially Weighted", "Equally Weighted"), col=c("green4","red", "blue"), lty=1:1, cex=1, lwd =1)


#Arm 2
plot(c(0:1000), Qmat_r1[,2], type = "l", col = "red", ylim = c(-1,1), xlab = "Stage", ylab = "Reward", main = "Arm 2")
lines(Qmat_r2[,2], type = "l", col = "blue")
lines(mumat[,2], type = "l", col = "green4")
legend("topright", legend=c("True", "Exponentially Weighted", "Equally Weighted"), col=c("green4","red", "blue"), lty=1:1, cex=1, lwd = 1)

# (c)
#Which arm had a better estimate of the optimal arm's mean? (measured by MSE)
best_arm = NULL 
for (i in 1:1000){
  best_arm[i] = which.max(mumat[i,])
}
Error_1 = mean((mumat[,best_arm] - Qmat_r1[-1,best_arm])^2)
Error_2 = mean((mumat[,best_arm] - Qmat_r2[-1,best_arm])^2)
which.min(c(Error_1,Error_2))

#Which run chose the optimal arm more often?
r1 = sum((best_arm == run_1[[3]]), na.rm = TRUE)
r2 = sum((best_arm == run_2[[3]]), na.rm = TRUE)
which.max(c(r1,r2))
## Exponentially weighted was more accurate.


# Question 3 ------------------------------------------
# (a)
action.preference.method= function(nplays,alpha,muvec){
  set.seed(2019)
  k = length(muvec)
  Hmat = matrix(NA, nrow = nplays+1, ncol=k) #matrix for our action preferences
  Hmat[1,] = 0 #we set H_1(a)=0,for all a=1,...,k
  
  pi1 = c(rep(1/k,k),rep(0,nplays*k)) # since H_1(a)=0,for all a, the initial probabilities are all equal
  Pimat = matrix(pi1,nrow=nplays+1, ncol=k, byrow = TRUE)  #Pi matrix 
  r = c(0,rep(NA,nplays)) #Vector for our rewards which will be updated based on which arm is sampled
  # first value of r is zero since rbar at time 1 is zero
  
  for(i in 2:(nplays+1)){
    a = sample(k,1,replace = TRUE, prob = Pimat[i-1,]) #choose action based on Pimat_(t-1)
    r[i] = rnorm(1,muvec[a],1) #we gerenerate a reward based on the distribution of the sampled arm
    rbar = mean(r[2:(i-1)])  #rbar = average r1,r2,...,r_(t-1)
    for(z in 1:k){ #for loop that will update Hmat
      if(z == a){
        Hmat[i,z] = Hmat[i-1,a]+alpha*(r[i]-rbar)*(1-Pimat[i-1,a]) #F-la for finding the next action preference if for the chosen arm
      }else{
        Hmat[i,z] = Hmat[i-1,z]-alpha*(r[i]-rbar)*(Pimat[i-1,z]) #F-la for finding the next action preference if for the non-chosen arms
      }
    }
    for(z in 1:k){ #for loop that will update our Pimat
      denom.pi = sum(exp(Hmat[i,1:k]))
      num.pi = exp(Hmat[i,z])
      Pimat[i,z] = num.pi/denom.pi
    }
  }
  result = list(Pimat, Hmat)
  return(result)
}

three.arms  = action.preference.method(1000,0.02,c(-0.5,0,0.5))
Pimat = three.arms[[1]]
Hmat = three.arms[[2]]

# (b)
plot(0:1000, Pimat[,1], type = "l", col = "green4", ylim = c(0,1), xlab = "Play", ylab = "Probability", main = "Action Preferences for 3 Arms", axes=FALSE)
lines(Pimat[,2],  type = "l", col = "red", lty = 1  )
lines(Pimat[,3],  type = "l", col = "blue", lty = 1 )
legend("right", legend=c(expression(paste("Arm 1,  ",mu," = -0.5")),
                         expression(paste("Arm 2,  ",mu," = 0")),
                         expression(paste("Arm 3,  ",mu," = 0.5"))),
       col=c("green4","red","blue"), lty= c(1,1,1), cex=1)
axis(2,at = seq(0,1, 0.25),labels=T)
axis(1,at=seq(0,1000,250),labels=T)





