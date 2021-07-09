setwd('/Users/jeremywalker/Desktop/298 Research Tortora/Scripts:Data')
data = read.csv('Maneuvers_attributes.xlsx - Sheet1 Copy.csv', header = TRUE, sep = ',')

#N total maneuvers crossed
data$N.total.maneuvers.crossed[is.na(data$N.total.maneuvers.crossed)] =  0
data$N.total.maneuvers.crossed.1[is.na(data$N.total.maneuvers.crossed.1)] = 0

data$N.total.maneuvers.crossed[data$N.total.maneuvers.crossed <= 2 & data$N.total.maneuvers.crossed > 0] = 1
data$N.total.maneuvers.crossed.1[data$N.total.maneuvers.crossed.1 <= 2 & data$N.total.maneuvers.crossed.1 > 0] = 1

data$N.total.maneuvers.crossed[data$N.total.maneuvers.crossed > 2] = 2
data$N.total.maneuvers.crossed.1[data$N.total.maneuvers.crossed.1 > 2] = 2

waiting_times_by_typo = read.csv('Three Waiting Times.csv', header = TRUE, sep = ',')

library('np')

###### Typology 1 ######
data_type1 = data.frame(maneuver_type=factor(data$Type.of.maneuver, labels = c(0,1,2)), 
                        lanes_edge_to=ordered(data$N.total.lanes.edge.to),
                        traffic_light=factor(data$Traffic.light, labels = c(0,1)), 
                        n_maneuvers_crossed=ordered(data$Merged.N.total.maneuvers.crossed),
                        connections_node=ordered(data$N.connection.at.node), 
                        critical_volume=data$Merged.critical.volume, 
                        avg_pce_flow=data$Average.PCE.flow.intersection,
                        length=data$Length.maneuver..m., 
                        response=waiting_times_by_typo$Average.waiting.time.TYPE.1)
npseed(71414)
bws = npregbw(formula = response ~ maneuver_type + lanes_edge_to + traffic_light + 
                n_maneuvers_crossed + connections_node + critical_volume + avg_pce_flow +
                length, regtype = 'lc', data = data_type1, okertype = 'liracine')
model = npreg(bws = bws)

bs_replication_1 = kernel_reg_boostrap(data = data_type1, bw_object = bws, B = 1000)
bs_mean_1 = apply(bs_replication_1, 2, mean)
bs_sd_1 = apply(bs_replication_1, 2, sd)

###### Typology 2 ######
data_type2 = data.frame(maneuver_type=factor(data$Type.of.maneuver, labels = c(0,1,2)), 
                        lanes_edge_to=ordered(data$N.total.lanes.edge.to),
                        traffic_light=factor(data$Traffic.light, labels = c(0,1)), 
                        n_maneuvers_crossed=ordered(data$Merged.N.total.maneuvers.crossed),
                        connections_node=ordered(data$N.connection.at.node), 
                        critical_volume=data$Merged.critical.volume, 
                        avg_pce_flow=data$Average.PCE.flow.intersection,
                        length=data$Length.maneuver..m., 
                        response=waiting_times_by_typo$Average.waiting.time.TYPE.2)

npseed(71414)
bws2 = npregbw(formula = response ~ maneuver_type + lanes_edge_to + traffic_light + 
                n_maneuvers_crossed + connections_node + critical_volume + avg_pce_flow +
                length, regtype = 'lc', data = data_type2, okertype = 'liracine')
model2 = npreg(bws = bws2)

bs_replication_2 = kernel_reg_boostrap(data = data_type2, bw_object = bws2, B = 1000)
bs_mean_2 = apply(bs_replication_2, 2, mean)
bs_sd_2 = apply(bs_replication_2, 2, sd)

###### Typology 3 ######
data_type3 = data.frame(maneuver_type=factor(data$Type.of.maneuver, labels = c(0,1,2)), 
                        lanes_edge_to=ordered(data$N.total.lanes.edge.to),
                        traffic_light=factor(data$Traffic.light, labels = c(0,1)), 
                        n_maneuvers_crossed=ordered(data$Merged.N.total.maneuvers.crossed),
                        connections_node=ordered(data$N.connection.at.node), 
                        critical_volume=data$Merged.critical.volume, 
                        avg_pce_flow=data$Average.PCE.flow.intersection,
                        length=data$Length.maneuver..m., 
                        response=waiting_times_by_typo$Average.waiting.time.TYPE.3)

npseed(71414)
bws3 = npregbw(formula = response ~ maneuver_type + lanes_edge_to + traffic_light + 
                 n_maneuvers_crossed + connections_node + critical_volume + avg_pce_flow + 
                 length, regtype = 'lc', data = data_type3, okertype = 'liracine')
model3 = npreg(bws = bws3)

bs_replication_3 = kernel_reg_boostrap(data = data_type3, bw_object = bws3, B = 1000)
bs_mean_3 = apply(bs_replication_3, 2, mean)
bs_sd_3 = apply(bs_replication_3, 2, sd)

