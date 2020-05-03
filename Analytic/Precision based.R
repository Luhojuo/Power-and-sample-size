#Precision based sample size calculation using 95% CI

library(Hmisc)
library(ggplot2)
library(plotly)

z<-1.96 #Determine confidence interval

##Function for the sample size and prevalence
PrevalSampSiAbs<-function(z,p,precision){ #Using absolute precision
	q <- 1-p
	m <- (precision*100)/100 #
	n <- (z^2 * p * q)/((m)^2)

	sampsi <- ceiling(n)


	return(list(Prevalence=p,N=sampsi))

}


##Make a basic plot
plot(runif(981,0,.2),runif(981,1,2000),type="n",xlab="Prevalence",
ylab="Sample size",xlim=c(0.01,.2),axes=TRUE)

minor.tick(nx=5,ny=5,  tick.ratio=.5)

SampleSiz<-c()
Prevalenc<-c()
Precisio<-c()

SampleSize<-c()
Prevalence<-c()
Precision<-c()


for(j in seq(0.005,0.05,by=0.00001)){
for(i in 1:length(seq(.01,.2,by=0.01))){

	p<-seq(.01,.2,by=0.01)[i]
	
	SampleSiz[i]<-PrevalSampSiAbs(1.96,p,j)$N
	Prevalenc[i]<-PrevalSampSiAbs(1.96,p,j)$Prevalence

}

Precisio<-rep(j,length(Prevalenc))

SampleSize<-c(SampleSize,SampleSiz)
Prevalence<-c(Prevalence,Prevalenc)
Precision<-c(Precision,Precisio)


color<-ifelse(j<1.5/100 & !is.na(j),"black",
			ifelse(j>=1.5/100 & j<2.5/100,"green",
			ifelse(j>=2.5/100 & j<3.5/100,"yellow",
			ifelse(j>=3.5/100,"red","white"))))

lines(Prevalenc,SampleSiz,col=color,lty=1)

}
text(0.05,1700,paste("Precision:"),col="white")
text(0.05,1600,paste("0.5-1.5%","%"),col="white")

text(0.10,1200,paste("Precision:"),col="black")
text(0.10,1100,paste("1.5-2.5","%"),col="black")

text(0.15,600,paste("Precision:"),col="black")
text(0.15,500,paste("2.5-3.5","%"),col="black")

text(0.16,300,paste("Precision:3.5-5 %"),col="white")

grid(nx = 4, ny = 4, col = "lightgray", lty = "dashed")




############################
## Plot with hover tooltip #
############################

picData<-cbind.data.frame(Precision,SampleSize,Prevalence)
picData$PrecisionRange<-ifelse(Precision<1.5/100 & !is.na(Precision),"0.5-1.5%",
			ifelse(Precision>=1.5/100 & Precision<2.5/100,"1.5-2.5%",
			ifelse(Precision>=2.5/100 & Precision<3.5/100,"2.5-3.5%",
			ifelse(Precision>=3.5/100,">3.5%","white"))))


pic<-ggplot(picData,aes(Prevalence,SampleSize,group=Precision))+
geom_line(aes(color=PrecisionRange))+ theme_bw()

picFixed<-pic+ scale_color_manual(values=c("red","black","green","yellow"))

ggplotly(picFixed)



