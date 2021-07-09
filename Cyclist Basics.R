#######################################
#######################################
###### Import Necessary Functions #####
#######################################
#######################################

weights <- function(data=NULL, gpar=NULL, v=1) {
  G = length(gpar$pi)
  if (G > 1) {
    fx = matrix(0, nrow=nrow(data), ncol=length(gpar$pi))
    for (k in 1:G ) fx[,k] = ddmsghyp(data=data, par=gpar[[k]] )
    
    w = t(apply(fx, 1, function(z,wt,v) {
      x= (z*wt)^v;
      
      if (sum(x)  == 0) x= rep(1,length(x))
      x =  x/sum(x)
      return( x )
    }, wt=gpar$pi,v=v ))
  } else w = matrix(1,nrow=nrow(data), ncol=G)
  return(w)
}

ddmsghyp <- function(data=NULL, par=NULL) {
  if (par$wg[2] != 0) {
    val1 = ddghyp(y=data,par=par,log=TRUE)
    val2 = dmsghyp(y=data,par=par,log=TRUE)
    val =  par$wg[1]*exp(val1) + par$wg[2]*exp(val2)
  } else val = ddghyp(y=data,par=par,log=FALSE)
  
  return( val )
}

ddghyp <- function(y=NULL, par=NULL, log=FALSE, invS=NULL) {
  # x  is a n * p matrix
  x     = y %*% (par$gam)
  mu = par$mu
  
  alpha = par$alpha
  
  if (length(mu) != length(alpha) ) stop("mu and alpha do not have the same length")
  d = length(mu)
  omega =  par$cpl0[1];
  lambda = par$cpl0[2];
  
  pa = omega + sum(alpha^2/par$phi )
  xmu = sweep(x,2,mu)
  mx = omega + apply( xmu^2, 1, weighted.sum, wt=1/par$phi)
  
  pa2=rep(pa,length(mx))
  kx = sqrt(mx * pa2)
  
  lvx = matrix(0, nrow=nrow(x), 4)
  lvx[,1] = (lambda - d/2)*log(kx)
  lvx[,2] = log(besselK( kx, nu=lambda-d/2, expon.scaled =TRUE)) - kx
  lvx[,3] = apply(xmu, 1, weighted.sum,  wt=alpha/par$phi )
  
  lv = numeric(6)
  lv[1] = -1/2*sum( log( par$phi) ) -d/2*(log(2)+log(pi))
  lv[2] =  omega - log(besselK( omega, nu=lambda, expon.scaled =TRUE))
  lv[3] = -lambda/2*( log(1) )
  lv[4] = lambda*log(omega) * 0
  lv[5] = (d/2 - lambda)*log( pa )
  
  val = apply(lvx,1,sum) + sum(lv)
  if (!log) val = exp( val )
  
  return(val)
}

dmsghyp <- function(y, par, log=FALSE) {
  # x is a n x p matrix
  x     = y %*% (par$gam)
  mu    = par$mu; phi = par$phi;
  alpha = par$alpha;
  d = length(mu); chi = par$cpl[,1]; psi = par$cpl[,1];
  lambda = par$cpl[,2];
  
  xmu = sweep(x,2,mu,"-")
  
  pa = psi + alpha^2/phi   # p numeric
  mx = sweep(sweep(xmu^2,2,1/phi, FUN="*"), 2,chi, "+") # n x p matrix
  kx = sqrt(sweep(mx, 2, pa, "*")) # nxp matrix
  
  lx1 = sweep( sweep(log(mx),2,log(pa),"-"), 2, (lambda - 1/2)/2, "*")
  lx2 = t(apply(kx, 1, function(z,lam=NULL) { log(besselK( z, nu=lambda-1/2, expon.scaled =TRUE)) - z }, lam=lambda ))
  lx3 = sweep(xmu, 2, alpha/phi, FUN="*")
  
  lv  = matrix(0, nrow=d, ncol=3)
  lv[,1] = - 1/2*log( phi ) - 1/2*(log(2)+log(pi)) # d=1
  lv[,2] = - (log(besselK( sqrt(chi*psi), nu=lambda, expon.scaled =TRUE)) - sqrt(chi*psi) )
  lv[,3] = lambda/2*(log(psi)-log(chi) )
  
  if(ncol(y)==1){lx2=t(lx2)}
  val = apply(lx1 + lx2 + lx3, 1, sum) + sum(lv)
  
  if (!log) val = exp( val )
  
  return(val)
}

weighted.sum <- function(z,wt,...) return( sum(z*wt,...) )

#######################################
#######################################
######## Classify New Cyclists ########
#######################################
#######################################

argmax = function(vec) {
  return(which(vec == max(vec)))
}

classify_new = function(data, par) {
  data = as.matrix(data)
  v_ig = weights(data = data, gpar = par)
  preds = apply(v_ig, 1, argmax)
  return(preds)
}

id_cluster = function(map, ID) {
  return (data.frame('ID' = ID, 'Cluster Membership' = map))
}

ID_by_cluster = write.csv(id_cluster(dat1_MCGHD_25@map, ID), file = 'ID_by_cluster.csv')

#####################################################
#####################################################
###### Exploratory Analysis for Categorization ######
#####################################################
#####################################################

setwd('/Users/jeremywalker/Desktop/298 Research Tortora/Scripts:Data')
data = read.csv('Maneuvers_attributes.xlsx - Sheet1 Copy.csv', header = TRUE, sep = ',')

#merged rank maneuver
barplot(height = c(sum(data$Merged.rank.maneuver==1), sum(data$Merged.rank.maneuver==2),sum(data$Merged.rank.maneuver==3),
                   sum(data$Merged.rank.maneuver==4)),
        xlab = 'Value', ylab = 'Count', main = 'Merged Rank Maneuver', names.arg = c('1','2','3','4'))
#0 for missing values, 1 stays 1, > 1 becomes 2; code as factor

#merged N total maneuvers crossed
barplot(height = c(sum(data$Merged.N.total.maneuvers.crossed==1), sum(data$Merged.N.total.maneuvers.crossed==2),sum(data$Merged.N.total.maneuvers.crossed==3),
                   sum(data$Merged.N.total.maneuvers.crossed==4),sum(data$Merged.N.total.maneuvers.crossed==5),sum(data$Merged.N.total.maneuvers.crossed==6),
                   sum(data$Merged.N.total.maneuvers.crossed==7),sum(data$Merged.N.total.maneuvers.crossed==8)),
        xlab = 'Value', ylab = 'Count', main = 'Number Maneuvers Crossed Histogram', names.arg = c('1','2','3','4','5','6','7','8'))
#0 for missing values, 1 and 2 becomes 1; > 2 becomes 2

#merged N bicycle maneuvers crossed
barplot(height = c(sum(data$Merged.N.bicycle.maneuvers.crossed==0), sum(data$Merged.N.bicycle.maneuvers.crossed==1),sum(data$Merged.N.bicycle.maneuvers.crossed==2),
                   sum(data$Merged.N.bicycle.maneuvers.crossed==3), sum(data$Merged.N.bicycle.maneuvers.crossed==4)),
        xlab = 'Value', ylab = 'Count', main = 'Merged N Total Bicycle Maneuvers Crossed', names.arg = c('0','1','2','3','4'))
#0 for missing values, 0 becomes 1, 1 becomes 2, >= 2 becomes 3

#merged critical volume
hist(data$Merged.critical.volume, breaks = c(0,250,500,750,1000,1250,1500,1750,2000), xaxt = 'n',
     xlab = 'Value', main = 'Merged Critical Volume', col = 'gray')
axis(side = 1, at = seq(0,2000,250))
#0 for missing values, between 0 and 250, 250 and 750, >750

########################################
########################################
###### Categorize Above Variables ######
########################################
########################################

#rank maneuver
data$Rank.maneuver[is.na(data$Rank.maneuver)] = 0; data$Rank.maneuver.1[is.na(data$Rank.maneuver.1)] = 0
data$Rank.maneuver[data$Rank.maneuver > 1] = 2; data$Rank.maneuver.1[data$Rank.maneuver.1 > 1] = 2

#N total maneuvers crossed
data$N.total.maneuvers.crossed[is.na(data$N.total.maneuvers.crossed)] =  0
data$N.total.maneuvers.crossed.1[is.na(data$N.total.maneuvers.crossed.1)] = 0

data$N.total.maneuvers.crossed[data$N.total.maneuvers.crossed <= 2 & data$N.total.maneuvers.crossed > 0] = 1
data$N.total.maneuvers.crossed.1[data$N.total.maneuvers.crossed.1 <= 2 & data$N.total.maneuvers.crossed.1 > 0] = 1

data$N.total.maneuvers.crossed[data$N.total.maneuvers.crossed > 2] = 2
data$N.total.maneuvers.crossed.1[data$N.total.maneuvers.crossed.1 > 2] = 2

#N bicycle maneuvers crossed
data$N.bycicle.maneuvers.crossed[data$N.bycicle.maneuvers.crossed >= 2] = 3
data$N.bycicle.maneuvers.crossed.1[data$N.bycicle.maneuvers.crossed.1 >= 2] = 3

data$N.bycicle.maneuvers.crossed[data$N.bycicle.maneuvers.crossed == 1] = 2
data$N.bycicle.maneuvers.crossed.1[data$N.bycicle.maneuvers.crossed.1 == 1] = 2

data$N.bycicle.maneuvers.crossed[data$N.bycicle.maneuvers.crossed == 0] = 1
data$N.bycicle.maneuvers.crossed.1[data$N.bycicle.maneuvers.crossed.1 == 0] = 1

data$N.bycicle.maneuvers.crossed[is.na(data$N.bycicle.maneuvers.crossed)] = 0
data$N.bycicle.maneuvers.crossed.1[is.na(data$N.bycicle.maneuvers.crossed.1)] = 0

#critical volume
data$Critical.volume[is.na(data$Critical.volume)] = 0
data$Critical.volume.1[is.na(data$Critical.volume.1)] = 0

data$Critical.volume[data$Critical.volume > 0 & data$Critical.volume <= 250] = 1
data$Critical.volume.1[data$Critical.volume.1 > 0 & data$Critical.volume.1 <= 250] = 1

data$Critical.volume[data$Critical.volume > 250 & data$Critical.volume <= 750] = 2
data$Critical.volume.1[data$Critical.volume.1 > 250 & data$Critical.volume.1 <= 750] = 2

data$Critical.volume[data$Critical.volume > 750] = 3
data$Critical.volume.1[data$Critical.volume.1 > 750] = 3

