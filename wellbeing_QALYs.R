# Graphs for wellbeing QALYs book chapter	
# 
# Author: Miqdad Asaria
# Date: 27 May 2016; revised 15 June 2016
###############################################################################
library(ggplot2)
library(grid)
library(gridExtra)

# function to calculate utility of consumption
u = function(c_it, c_min){
	eta = 1.26
	c_std = 30000
	B = 1 / (c_min^(1-eta) - c_std^(1-eta))
	A = c_min^(1-eta)*B
	u_it = A-B*c_it^(1-eta)
	return(u_it)
}

# wellbeing as a function of health, consumption and alpha 
w = function(h_it, c_it, alpha, c_min){
	w_it = alpha + alpha*h_it*u(c_it, c_min) + (1-alpha)*(h_it+u(c_it, c_min)) - 1
	return(w_it)
}

make_consumption_graph_data = function(c_it, health, alpha, c_min){
	w_h = list()
	for(i in 1:length(health)){
		w_h = append(w_h,w(health[i],c_it,alpha, c_min))
	}
	x = rep(log(c_it),length(health))
	y = sapply(w_h, identity)
	health_group = as.factor(rep(paste("health=",health,sep=""),each=length(c_it)))
	consumption_graph_data = data.frame(x,y,health_group,alpha=paste("alpha=",alpha,sep=""))
	
	return(consumption_graph_data)
}

make_health_graph_data = function(h_it, consumption, alpha, c_min){	
	w_c = list()
	for(i in 1:length(consumption)){
		w_c = append(w_c,w(h_it,consumption[i],alpha,c_min))
	}
	x = rep(h_it,length(consumption))
	y = sapply(w_c, identity)
	consumption_group = as.factor(rep(paste("consumption=",consumption,"USD",sep=""),each=length(h_it)))
	health_graph_data = data.frame(x,y,consumption_group,alpha=paste("alpha=",alpha,sep=""))
	
	return(health_graph_data)
}


plot_wellbeing_graphs_panel = function(alpha, c_min){
	## for consumption graphs
	# consumption levels in USD
	c_it = c(300,1000,3000,10000,30000,100000,300000,1000000,3000000,10000000)
	# health in QALYs
	health = c(0,0.5,1)
	
	## for health graphs
	# health in QALYs
	h_it = c(-0.281,0,0.25,0.5,0.75,1)
	# consumption levels in USD
	consumption = c(300,30000,10000000)
	
	consumption_graph_data = list()
	health_graph_data = list()
	for(a in 1:length(alpha)){
		consumption_graph_data = rbind(consumption_graph_data,make_consumption_graph_data(c_it, health, alpha[a], c_min))
		health_graph_data = rbind(health_graph_data,make_health_graph_data(h_it, consumption, alpha[a], c_min))
	}
	
	# plot consumption graph, by health level, panel by alpha
	consumption_panel = ggplot(consumption_graph_data, aes(x=x, y=y,group=health_group,colour=health_group, linetype=health_group)) +
			geom_line() +
			xlab("Income in USD") + 
			ylab("Wellbeing") +
			facet_wrap(~alpha) +
			scale_x_continuous(breaks=log(c_it),labels=c("300","1k","3k","10k","30k","100k","300k","1m","3m","10m")) +
			scale_color_manual("Health Quality",values=c("red","green","blue"),labels=c("0","0.5","1")) +
			scale_linetype_manual("Health Quality",values=c(1,2,3),labels=c("0","0.5","1")) +
			geom_vline(xintercept = log(30000),linetype=2,colour="grey") +
			geom_hline(yintercept = 0,linetype=2,colour="grey") +
	    geom_hline(yintercept = 1,linetype=2,colour="grey") +
	  theme_bw() +
			theme(panel.grid.major = element_blank(),
					panel.grid.minor = element_blank(),
					panel.background = element_blank())
	
	# plot health graph, by consumption level, panel by alpha
	health_panel = ggplot(health_graph_data, aes(x=x, y=y,group=consumption_group,colour=consumption_group, linetype=consumption_group)) +
			geom_line() +
			xlab("Health Quality") + 
			ylab("Wellbeing") +
			facet_wrap(~alpha) +
			scale_x_continuous(breaks=h_it,labels=h_it) +
			scale_color_manual("Income",values=c("red","green","blue"),labels=c("10 million","30 thousand","300")) +
			scale_linetype_manual("Income",values=c(1,2,3),labels=c("10 million","30 thousand","300")) +
			geom_vline(xintercept = 0,linetype=2,colour="grey") +
			geom_hline(yintercept = 0,linetype=2,colour="grey") +
	    geom_hline(yintercept = 1,linetype=2,colour="grey") +
	  theme_bw() +
			theme(panel.grid.major = element_blank(),
					panel.grid.minor = element_blank(),
					panel.background = element_blank())
	
	plots = arrangeGrob(consumption_panel,health_panel,nrow=2)
	
	ggsave(paste("GBD_graph_panel_c_min_",c_min,".png", sep=""), plots, device="png", width = 65, height = 40, units="cm", dpi = 300)
}

plot_c_min_scenarios = function(alpha, c_mins){
	## for consumption graphs
	# consumption levels in USD
	c_it = c(300,1000,3000,10000,30000,100000,300000,1000000,3000000,10000000)
	# health in QALYs
	health = 1
	

	consumption_graph_data = list()
	for(c_min in c_mins){
		consumption_graph_data = rbind(consumption_graph_data,cbind(c_min,make_consumption_graph_data(c_it, health, alpha, c_min)))
	}
	consumption_graph_data$c_min = as.factor(consumption_graph_data$c_min)
	set.seed(123)
	# plot consumption graph, by health level, panel by alpha
	consumption_panel = ggplot(consumption_graph_data, aes(x=x, y=y,group=c_min,colour=c_min, linetype=c_min)) +
			geom_line() +
			xlab("Income in USD") + 
			ylab("Wellbeing") +
			scale_x_continuous(breaks=log(c_it),labels=c("300","1k","3k","10k","30k","100k","300k","1m","3m","10m")) +
			scale_color_manual("c_min",values=sample(colors(), length(c_mins), replace=FALSE),labels=c_mins) +
			scale_linetype_manual("c_min",values=c(1,sample(2:10, length(c_mins)-1, replace=FALSE)),labels=c_mins) +
			geom_vline(xintercept = log(30000),linetype=2,colour="grey") +
			geom_hline(yintercept = 0,linetype=2,colour="grey") +
			geom_hline(yintercept = 1,linetype=2,colour="grey") +
			theme_bw() +
			theme(panel.grid.major = element_blank(),
					panel.grid.minor = element_blank(),
					panel.background = element_blank())
	
	ggsave(paste("GBD_graph_c_min_scenarios_(",toString(c_mins),").png", sep=""), consumption_panel, device="png", width = 23, height = 20, units="cm", dpi = 300)
}

# plot graphs
plot_wellbeing_graphs_panel(alpha=c(-1,0,0.5), c_min=300)
plot_wellbeing_graphs_panel(alpha=c(-1,0,0.5), c_min=10000)
plot_c_min_scenarios(alpha=1, c_mins=c(300,10000))
plot_c_min_scenarios(alpha=1, c_mins=c(300))



