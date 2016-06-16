# Shiny ui for wellbeing adjusted life years example
# 
# Author: Miqdad Asaria
# Date: 16/06/2016
###############################################################################

library(shiny)

shinyUI(pageWithSidebar(
			
				# Application title
				headerPanel("Wellbeing Adjusted Life Years"),
				
				sidebarPanel(
						sliderInput("c_min", "Minimum Income Level", min=0, max=30000, value=300),
						
						selectInput("health", "Health Quality:",
								list("Dead (0)" = 0, 
										"Sick (0.5)" = 0.5, 
										"Full health (1)" = 1), selected=1),
			
						selectInput("alpha", "Alpha:",
								list("-1" = -1, 
										"0" = 0, 
										"0.5" = 0.5), selected=0)
						),
				
				mainPanel(   
					h3(textOutput("caption")),
					plotOutput("consPlot")
				)
))