# Shiny server for wellbeing adjusted life years example
# 
# Author: Miqdad Asaria
# Date: 16/06/2016
###############################################################################

library(shiny)
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
	consumption_graph_data = data.frame(x,y)
	
	return(consumption_graph_data)
}

plot_c_min_scenario = function(alpha, health, c_min){
	## for consumption graphs
	# consumption levels in USD
	c_it = c(300,1000,3000,10000,30000,100000,300000,1000000,3000000,10000000)	
	
	consumption_graph_data = make_consumption_graph_data(c_it, health, alpha, c_min)

	# plot consumption graph, by health level, panel by alpha
	consumption_graph = ggplot(consumption_graph_data, aes(x=x, y=y)) +
			geom_line() +
			xlab("Income in USD") + 
			ylab("Wellbeing") +
			scale_x_continuous(breaks=log(c_it),labels=c("300","1k","3k","10k","30k","100k","300k","1m","3m","10m")) +
			geom_vline(xintercept = log(30000),linetype=2,colour="grey") +
			geom_hline(yintercept = 0,linetype=2,colour="grey") +
			geom_hline(yintercept = 1,linetype=2,colour="grey") +
			theme_bw() +
			theme(panel.grid.major = element_blank(),
					panel.grid.minor = element_blank(),
					panel.background = element_blank())
	return(consumption_graph)
}

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
	output$caption = renderText({
			paste("Minimum Income Level = ", input$c_min, ", Alpha = ", input$alpha, ", Health quality = ", input$health)
	})
			
	output$consPlot = renderPlot({
		print(plot_c_min_scenario(as.double(input$alpha), as.double(input$health), as.integer(input$c_min)))
	})
})
