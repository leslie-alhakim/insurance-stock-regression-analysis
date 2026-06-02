# Leslie Alhakim
# BUA 633 Term Project 1

# Import packages
library(YRmisc)

# Import datasets 
library(readxl)
spInfo <- read_excel("Downloads/BUA 633/spInfo-1 (1).xlsx")
View(spInfo)

spData <- read_excel("Downloads/BUA 633/spData-1 (1).xlsx")
View(spData)

# Preliminaries
names(spData)
names(spInfo)

data.class(spData)
spData<-as.data.frame(spData)
data.class(spData)

data.class(spInfo)
spInfo<-as.data.frame(spInfo)
data.class(spInfo)

# Merge dataframes
spdf<-merge(spData,spInfo,by="tkr")
dim(spdf)
names(spdf)

# Extract "year" variable
spdf$date<-as.numeric(substring(spdf$date,7,10))
names(spdf)[2]<-"year"
names(spdf)

# Cross section analysis
max(spdf$year)
csdf<-spdf[spdf$year==2022,]
names(csdf);dim(csdf);
data.frame(table(csdf$industry))

# Choose an industry: INSURANCE
idf<-csdf[csdf$industry=="Insurance",
          c("tkr","price","eps","bvps","cr","dta")]
idf; dim(idf)

# Graphical techniques: HISTOGRAMS
histogram <-function(x,myTitle,xxlab,yylab){
  hist(x,main=myTitle,xlab=xxlab,ylab=yylab)}

par(mfrow=c(3,3))
histogram(idf$price,"Fig. 1 Hist of Price", "Price","Frequency")
histogram(idf$eps,"Fig. 2 Hist of EPS", "EPS","Frequency")
histogram(idf$bvps,"Fig. 3 Hist of BVPS", "BVPS","Frequency")
histogram(idf$cr,"Fig. 4 Hist of CR", "CR","Frequency")
histogram(idf$dta,"Fig. 5 Hist of DTA", "DTA","Frequency")

# Graphical techniques: SCATTERPLOTS
plotTkr<-function(x,y,z,xxlab,yylab,myTitle){
  scatter.smooth(x,y,type="n",xlab=xxlab,ylab=yylab,main=myTitle)
  text(x,y,z,cex=.6)  }

par(mfrow=c(2,2))
plotTkr(idf$eps,idf$price,idf$tkr,"EPS","Price","Fig. 6 Scatterplot of Price v EPS")
plotTkr(idf$bvps,idf$price,idf$tkr,"BVPS","Price","Fig. 7 Scatterplot of Price v BVPS")
plotTkr(idf$cr,idf$price,idf$tkr,"CR","Price","Fig. 8 Scatterplot of Price v CR")
plotTkr(idf$dta,idf$price,idf$tkr,"DTA","Price","Fig. 9 Scatterplot of Price v DTA")

# Analytical methods: DESCRIPTIVE STATISTICS
ds.summ(idf[,c("price","eps","bvps","cr","dta")],2)

# Analytical methods: CORRELAION
round(cor(idf[,c("price","eps","bvps","cr","dta")]),3)

# Analytical methods: REGRESSION RESULTS
fit<-lm(price ~ eps+bvps+cr+dta,na.action=na.omit,data=idf)
summary(fit)

# Post regression validation
par(mfcol=c(2,2))
hist(fit$residuals, main ="Fig. 10 Hist of Residuals")
plotTkr(fit$fitted.values,fit$model$price,idf$tkr, "Predicted Price","Actual Price","Fig. 11 Act v Pred Price")

# Graphical methods: NONLINEAR, NONPARAMETRIC REGRESSION
library(gam)
fit<-gam(price~eps+bvps+cr+dta,na.action=na.omit,data=idf)

cor(fit$model$price,fit$fitted.values)^2