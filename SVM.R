########################################
########################################
############ Play with SVM #############
########################################
########################################

#perform data importation and variable manipulation as described in Feature Selection.R

data_type1 = data.frame(maneuver_type=factor(data$Type.of.maneuver, labels = c(0,1,2)),
                        lanes_edge_to=ordered(data$N.total.lanes.edge.to),
                        traffic_light=factor(data$Traffic.light, labels = c(0,1)),
                        n_maneuvers_crossed=ordered(data$Merged.N.total.maneuvers.crossed),
                        connections_node=ordered(data$N.connection.at.node),
                        critical_volume=data$Merged.critical.volume,
                        avg_pce_flow=data$Average.PCE.flow.intersection,
                        length=data$Length.maneuver..m.,
                        response=waiting_times_by_typo$Average.waiting.time.TYPE.1)

waiting_times_by_typo = read.csv('Three Waiting Times.csv', header = TRUE, sep = ',')

###### SVM for Average Waiting Time (Type 1) ######

#categorize the response (average waiting time) for classification
y = data_type1$response
y[y > 2.5] = 3
y[y > 0.75 & y < 2.5] = 2
y[y >= 0 & y <= 0.75] = 1
data_type1$response = y

set.seed(71414)
train_index = sample(seq(1,60,1), 48, replace = FALSE)
test_index = seq(1,60,1); test_index = test_index[-train_index]

X_train = X[train_index,]; X_test = X[test_index,]
y_train = y[train_index]; y_test = y[test_index]

#gaussian kernel sigma parameter auto-tuner (Dr. Chen)
# library('FNN')
# rbf_sigma_tune = function(X_train, y_train, n_neighbors) {
#   dist_sum = 0
#   for(label in unique(y_train)) {
#     subset = X_train[y_train == label, ]
#     knn = get.knn(subset, k = n_neighbors)
#     dist_sum = dist_sum + sum(knn$nn.dist[,n_neighbors])
#   }
#   sigma = (1/nrow(X_train))*dist_sum
#   gam = 1/(2*sigma^2)
#   return(gam)
# }

#parameter tune the regularization constant
library('e1071')
scores = c()
for(c in c(1e-5,1e-4,1e-3,1e-2,1e-1,1,1e1,1e2,1e3,1e4,1e5)) {
  SVM = svm(response ~., data = data_type1, type = 'C-classification', kernel = 'radial', cost = c)
  y_pred = predict(SVM, X_test); score = sum(y_pred == y_test) / length(y_test)
  scores = c(scores, score)
}
#appears that 1e2 is optimal

scores2 = c()
for(gam in c(1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1,1e1,1e2)) {
  SVM = svm(response ~., data = data_type1, type = 'C-classification', gamma = gam, kernel = 'radial', cost = 1e3)
  y_pred = predict(SVM, X_test); score = sum(y_pred == y_test) / length(y_test)
  scores2 = c(scores2, score)
}
#appears that 1e-2 is optimal

#now let's examine sensitivity to train/test split
scores3 = c()
for(i in 1:100) {
  train_index = sample(seq(1,60,1), 48, replace = FALSE)
  test_index = seq(1,60,1); test_index = test_index[-train_index]
  X_train = X[train_index,]; X_test = X[test_index,]
  y_train = y[train_index]; y_test = y[test_index]
  
  SVM = svm(response ~., data = data_type1, type = 'C-classification', gamma = 1e-2, kernel = 'radial', cost = 1e3)
  y_pred = predict(SVM, X_test); score = sum(y_pred == y_test) / 12
  scores3 = c(scores3, score)
}
#a little sensitive to train/test split, robustness is therefore a concern given Poliziani's goals

