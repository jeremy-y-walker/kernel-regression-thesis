library('Matrix')

setwd('/Users/jeremywalker/Desktop/298 Research Tortora/Scripts:Data')
data = read.csv('Maneuvers_attributes.xlsx - Sheet1 Copy.csv', header = TRUE, sep = ',')

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

waiting_times_by_typo = read.csv('Three Waiting Times.csv', header = TRUE, sep = ',')

complete_data_typeI = data.frame(maneuver_type=factor(data$Type.of.maneuver, labels = c(0,1,2)), 
                                 #as.factor(data$N.total.lanes.edge.from), 
                                 lanes_edge_to=as.factor(data$N.total.lanes.edge.to),
                                 #as.factor(data$N.bicycle.lanes.edge.from), 
                                 #as.factor(data$N.bicycle.lanes.edge.to), 
                                 #as.factor(data$Width.edge.from), 
                                 traffic_light=factor(data$Traffic.light, labels = c(0,1)), 
                                 rank_maneuver=as.factor(data$Merged.rank.maneuver), 
                                 n_maneuvers_crossed=as.factor(data$Merged.N.total.maneuvers.crossed),
                                 n_bicycle_crossed=as.factor(data$Merged.N.bicycle.maneuvers.crossed), 
                                 critical_volume=data$Merged.critical.volume, 
                                 avg_pce_flow=data$Average.PCE.flow.intersection,
                                 #data$Bus.flow.on.maneuver.analysed, 
                                 #data$Car.flow.on.maneuver.analysed, 
                                 #data$Bike.flow.on.maneuver.analysed,
                                 #data$Moto.flow.on.maneuver.analysed, 
                                 length=data$Length.maneuver..m., 
                                 connections_node=as.factor(data$N.connection.at.node), 
                                 response=waiting_times_by_typo$Average.waiting.time.TYPE.1)

categorical_data_type1 = data.frame(maneuver_type=as.numeric(complete_data_typeI$maneuver_type),
                                    lanes_edge_to=as.numeric(complete_data_typeI$lanes_edge_to),
                                    traffic_light=as.numeric(complete_data_typeI$traffic_light),
                                    rank_maneuver=as.numeric(complete_data_typeI$rank_maneuver),
                                    n_maneuvers_crossed=as.numeric(complete_data_typeI$n_maneuvers_crossed),
                                    n_bicycle_crossed=as.numeric(complete_data_typeI$n_bicycle_crossed),
                                    connections_node=as.numeric(complete_data_typeI$connections_node))

chisq_pval = function(chisq_stat, n1, n2) {
  return(pchisq(chisq_stat, df = (n1-1)*(n2-1), lower.tail = FALSE))
}

chisq_stat_matrix = matrix(NA, nrow = ncol(categorical_data_type1), ncol = ncol(categorical_data_type1))
chisq_pval_matrix = matrix(NA, nrow = ncol(categorical_data_type1), ncol = ncol(categorical_data_type1))

for(i in 1:ncol(categorical_data_type1)) {
  for(j in 1:ncol(categorical_data_type1)) {
    contin_table = table(categorical_data_type1[,i], categorical_data_type1[,j])
    n1 = nrow(contin_table); n2 = ncol(contin_table)
    stat = chisq.test(contin_table, simulate.p.value = TRUE)$statistic
    chisq_stat_matrix[i,j] = stat
    chisq_pval_matrix[i,j] = chisq_pval(stat, n1, n2)
  }
}

phiSq_stat_matrix = sqrt(chisq_stat_matrix / nrow(categorical_data_type1))

correlated_vars = matrix(NA, ncol = 2)
for(i in 1:7) {
  for(j in 1:i) {
    if(phiSq_stat_matrix[i,j] > 1) {
      if(i != j) {
        print(c(i,j))
      }
    }
  }
}
#correlated pairs: 5 and 4, 6 and 4, 6 and 5
#let's get rid of 4 and 6

corr_matrix = cor(scale(complete_data_typeI[,c(7,8,9)]))
#no highly correlated continuous predictors

#feature selection has indicated that variables:
  #maneuver_type
  #lanes_edge_to
  #traffic_light
  #n_maneuvers_crossed
  #connections_node
  #critical_volume
  #avg_pce_flow
  #length
#will be used for analysis
