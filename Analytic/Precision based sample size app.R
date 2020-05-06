##LAKANA COVID-19
#Precision based sample size calculation using 95% CI

library(Hmisc)
library(ggplot2)
library(plotly)
library(shiny)
library(markdown)

##Make vectors for Precision, Prevalence and Sample Size
Precision<-sort(rep(seq(0.005,0.05,by=0.00001),20))
Prevalence<-rep(seq(.01,.2,by=0.01),length(seq(0.005,0.05,by=0.00001)))
color<-ifelse(prec<1.5/100 & !is.na(Precision),"black",
			ifelse(Precision>=1.5/100 & Precision<2.5/100,"green",
			ifelse(Precision>=2.5/100 & Precision<3.5/100,"yellow",
			ifelse(Precision>=3.5/100,"red","white"))))

z<-1.96

SampleSize<-ceiling(((z^2)*Prevalence*(1-Prevalence))/((Precision)^2))


ui <-navbarPage("Power and Sample Size",
		tabPanel("Precision",
			sidebarLayout(
				sidebarPanel(

					sliderInput("k", "Coefficient of variation",
						min=0.001, max=1, value=0.001),

					sliderInput("mc", "Average cluster size",
						min=1, max=1000, value=0.001)

				),	
				mainPanel(
					splitLayout(cellWidths = c("50%", "50%"), plotOutput("Static"), plotlyOutput("Interactive"))

				)
			)
		)
)

server <- function(input, output) {


output$Static <- renderPlot({
	icc<-(input$k^2)*(Prevalence/(1-Prevalence))
	DEFF<-1+(input$mc-1)*icc
	SampSi <- DEFF*SampleSize

	plot(seq(0,.20,length.out=100),seq(0,2000,length.out=100),type="n",
	xlab="Prevalence", ylab="Sample Size")
	minor.tick(nx=5,ny=5,  tick.ratio=.5)


	for(i in seq(1,length(Prevalence),by=20)){
	  lines(Prevalence[i:(i+19)],SampSi[i:(i+19)],col=color[i])
	}
})

output$Interactive <- renderPlotly({
	icc<-(input$k^2)*(Prevalence/(1-Prevalence))
	DEFF<-1+(input$mc-1)*icc
	SampleSize <- ceiling(DEFF*SampleSize)
	picData<-cbind.data.frame(Precision,SampleSize,Prevalence)
	picData$PrecisionRange<-ifelse(Precision<1.5/100 & !is.na(Precision),"0.5-1.5%",
			ifelse(Precision>=1.5/100 & Precision<2.5/100,"1.5-2.5%",
			ifelse(Precision>=2.5/100 & Precision<3.5/100,"2.5-3.5%",
			ifelse(Precision>=3.5/100,"3.5%<","white"))))

	pic<-ggplot(picData,aes(Prevalence,SampleSize,group=Precision))+
	geom_line(aes(color=PrecisionRange))+ theme_bw()

	picFixed<-pic+ scale_color_manual(values=c("black","green","yellow","red"))

	ggplotly(picFixed)
})

}



shinyApp(ui = ui, server = server)






