########################################
########################################
###### Play with Random Forest ########
########################################
########################################

#perform data importation and variable manipulation as described in Feature Selection.R

waiting_times_by_typo = read.csv('Three Waiting Times.csv', header = TRUE, sep = ',')

data_type1 = data.frame(maneuver_type=factor(data$Type.of.maneuver, labels = c(0,1,2)), 
                        lanes_edge_to=ordered(data$N.total.lanes.edge.to),
                        traffic_light=factor(data$Traffic.light, labels = c(0,1)), 
                        n_maneuvers_crossed=ordered(data$Merged.N.total.maneuvers.crossed),
                        connections_node=ordered(data$N.connection.at.node), 
                        critical_volume=data$Merged.critical.volume, 
                        avg_pce_flow=data$Average.PCE.flow.intersection,
                        length=data$Length.maneuver..m., 
                        response=waiting_times_by_typo$Average.waiting.time.TYPE.1)

mse_func = function(ypred, ytrue) {
  return((sum((ypred-ytrue)^2))/length(y_pred))
}

library('randomForest')

rf_scores = c()
for(i in seq(10,400,10)) {
  rf_model = randomForest(x = data_type1[,-9], y = data_type1[,9], ntree = i)
  y_pred = predict(rf_model, data_type1[,-9])
  rf_scores = c(rf_scores, mse_func(y_pred,data_type1$response))
}

rf_scores2 = c()
for(i in seq(1,10)) {
  rf_model = randomForest(x = data_type1[,-9], y = data_type1[,9], ntree = 60, nodesize = i)
  y_pred = predict(rf_model, data_type1[,-9])
  rf_scores2 = c(rf_scores2, mse_func(y_pred,data_type1$response))
}

rf_final = randomForest(x = data_type1[,-9], y = data_type1[,9], ntree = 60, nodesize = 1)
y_pred = predict(rf_final, data_type1[,-9])
mse_func(y_pred, data_type1$response)
