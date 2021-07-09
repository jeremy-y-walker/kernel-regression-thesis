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

