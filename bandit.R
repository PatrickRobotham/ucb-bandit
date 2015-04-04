# Simulation for UCB Bandit, A/B test.

# A has true conversion rate of 0.1, B of 0.2
prob.conversion <- c(0.1,0.2)
n = 1000000
num.sims = 1

get_conversion <- function(version){
  return(runif(1) < prob.conversion[version])
}

# Z-test translated to R
# X = A, Y = B
getPValueZtest <- function(x_visitors, x_conversions, y_visitors, y_conversions){
  if (x_visitors < 5 || x_conversions < 5 || y_visitors < 5 || y_conversions < 5){
    return(1)
  }
  mean_x = x_conversions/x_visitors
  mean_y = y_conversions/y_visitors
  sd_x = sqrt(mean_x*(1-mean_x))
  sd_y = sqrt(mean_y*(1-mean_y))
  se_x = sd_x/sqrt(x_visitors)
  se_y = sd_y/sqrt(y_visitors)
  average_conversion_rate = (x_conversions+y_conversions)/(x_visitors+y_visitors)
  combined_se = sqrt(average_conversion_rate *
                     (1-average_conversion_rate) * 
                       ((1/x_visitors) + (1/y_visitors)))
  if (combined_se==0){
    return(1)
  }
  z_score = (mean_y - mean_x)/combined_se
  p_value = 2*(1-pnorm(abs(z_score)))
  return(p_value)
}

# Three different A/B testing algorithms.
# To make things simpler, 
# I initialize all algorithms with 1 visit and 1 conversion for each version.


# A/B testing, randomized version

abrandom <- function(num_trials){
  visitors <- c(1,1)
  conversions <- c(1,1)
  for (i in 1:num_trials){
  # Pick a version 
    version <- floor(runif(n=1,min=1.5,max=2.5))
  # Check if we got a conversion
    visitors[version] <- visitors[version] + 1
    conversions[version] <- conversions[version] + get_conversion(version)
  }
  # Return p value and total number of conversions
  return(list(p=getPValueZtest(x_visitors = visitors[1],
                               x_conversions = conversions[1],
                               y_visitors =visitors[2],
                               y_conversions = conversions[2]),
              totalConversions = sum(conversions)
  ))
}


#A/B Testing: Epsilon Greedy Version
abgreedy <- function(num_trials,epsilon){
  visitors <- c(1,1)
  conversions <- c(1,1)
  for (i in 1:num_trials){
    # Pick a version 
    if (runif(1) < epsilon){
      #explore
      version <- floor(runif(n=1,min=1.5,max=2.5))
    } else {
      # exploit
      version <- order(conversions/visitors,decreasing=TRUE)[1]
    }
    # Check if we got a conversion
    visitors[version] <- visitors[version] + 1
    conversions[version] <- conversions[version] + get_conversion(version)
  }
  # Return p value and total number of conversions
  return(list(p=getPValueZtest(x_visitors = visitors[1],
                               x_conversions = conversions[1],
                               y_visitors =visitors[2],
                               y_conversions = conversions[2]),
              totalConversions = sum(conversions)
  ))
}

# A/B Testing with UCB bandit 
abucb <- function(num_trials){
  
  visitors <- c(1,1)
  conversions <- c(1,1)
  for (i in 1:num_trials){
    # Pick a version using UCB1 criteria
    aUCB <- conversions[1]/visitors[1] + sqrt(2 * log(i) / visitors[1])
    bUCB <- conversions[2]/visitors[2] + sqrt(2 * log(i) / visitors[2])
    version <- order(c(aUCB,bUCB),decreasing=TRUE)[1]
    
    # Check if we got a conversion
    visitors[version] <- visitors[version] + 1
    conversions[version] <- conversions[version] + get_conversion(version)
  }
  # Return p value and total number of conversions
  return(list(p=getPValueZtest(x_visitors = visitors[1],
                               x_conversions = conversions[1],
                               y_visitors =visitors[2],
                               y_conversions = conversions[2]),
              totalConversions = sum(conversions)
  ))
}

simulateRandom  = data.frame(t(replicate(num.sims,abrandom(n))))
simulateRandom$p = unlist(simulateRandom$p)
simulateRandom$totalConversions = unlist(simulateRandom$totalConversions)

simulateUCB = data.frame(t(replicate(num.sims,abucb(n))))
simulateUCB$p = unlist(simulateUCB$p)
simulateUCB$totalConversions = unlist(simulateUCB$totalConversions)


simulateGreedy5050 = data.frame(t(replicate(num.sims,abgreedy(n,0.5))))
simulateGreedy5050$p = unlist(simulateGreedy5050$p)
simulateGreedy5050$totalConversions = unlist(simulateGreedy5050$totalConversions)

simulateGreedy9010 = data.frame(t(replicate(num.sims,abgreedy(n,0.1))))
simulateGreedy9010$p = unlist(simulateGreedy9010$p)
simulateGreedy9010$totalConversions = unlist(simulateGreedy9010$totalConversions)

print(paste("A/B Testing Results - n:" , n , "num.sims: " ,num.sims))
print("Random")
print(paste("Conversion Rate:",mean(simulateRandom$totalConversions)/n))
print(paste("Signifance Percentage:",sum(simulateRandom$p < 0.05) / num.sims))

print("UCB")
print(paste("Conversion Rate:",mean(simulateUCB$totalConversions)/n))
print(paste("Signifance Percentage:",sum(simulateUCB$p < 0.05) / num.sims))

print("Greedy - epsilon=0.5")
print(paste("Conversion Rate:",mean(simulateGreedy5050$totalConversions)/n))
print(paste("Signifance Percentage:",sum(simulateGreedy5050$p < 0.05) / num.sims))


print("Greedy - epsilon=0.1")
print(paste("Conversion Rate:",mean(simulateGreedy9010$totalConversions)/n))
print(paste("Signifance Percentage:",sum(simulateGreedy9010$p < 0.05) / num.sims))