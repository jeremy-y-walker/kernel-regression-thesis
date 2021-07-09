#This function uses boostrapping to replicate the estimates from the Nadaraya-Watson mixed data type nonparametric regression
#model implemented in R with the np package. The argument data should be a dataframe to accomodate mixed data types (nominal,
#ordinal, continuous) with rows corresponding to observations. bw_object should be an object of class 'rbandwidth' created
#with the np package function npregbw which is then used to define the npreg (nonparametric regression model) of which you want
#to estimate the standard errors of. B is the number of replications you would like (heads up; this is a computationally intensive task).
#The function returns a B x nrow(data) matrix where each row is one replication of all points of estimation; to estimate standard error
#apply the sd function along each column.

#Note: A bootstrap replication will omit ~36% of data so this function can also establish robustness by creating a "train/test split"
#for each replication"

library('np')

kernel_reg_boostrap = function(data, bw_object, B) {
  mean_eval = matrix(NA, nrow = B, ncol = nrow(data))
  
  for(i in 1:B) {
    bs_replica = data[sample(seq(1,nrow(data)), size = nrow(data), replace = TRUE),]
    bws = npregbw(formula = bw_object$formula, regtype = 'lc', data = bs_replica, okertype = 'liracine')
    model = npreg(bws = bws) 
    mean_eval[i,] = predict(model, newdata = data)
  }
  return(mean_eval)
}

