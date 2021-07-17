library(pdfetch)
library(readxl)
library(xts)
library(ggplot2)
library(ggcorrplot)
library(moments)
library(scales)
library(corrplot)
library(PerformanceAnalytics)
library(gridExtra)
library(car)
library(psych)
library(lmtest)
library(sandwich)
library(stats)
library(tidyverse)
library(glue)
library(dplyr)
library(lubridate)
library(anytime)

############
### DATA ##
###########

tickers = c("QQQ", "XLK", "VGT", "FDN","IYW","^GSPC","^IRX")
data = pdfetch_YAHOO(tickers,fields="adjclose",from="2014-01-01", to="2019-10-01", interval= "1wk")
names(data)=c("QQQ","XLK","VGT","FDN","IYW","SP500","rf")

nrow(data)

DJ_tech_index <- read_excel("C:/Users/Song/Desktop/DJ Index/DJ tech index.xls",skip = 4) 
DJ<- data.frame(na.omit(DJ_tech_index[,c(1,3)])) # select conlumn 1,which is dates,3,which is DJ index and remove NA data
names(DJ) <- c("Date","DJ") # change the column's names
DJ <- xts(DJ$DJ,as.Date(DJ$Date, format='%Y-%m-%d')) # switch dataframe to time series
#set the sequences including the dates we need to match the exact day with other data
dates <- seq(as.Date("2014-01-01"), to=as.Date("2019-10-01"), by="weeks") 
# the dates as the index 
DJ <- DJ[c(dates),] 

alldata <- na.omit(cbind(data[,seq(1,to=6)],DJ,data[,7])) # all data 

#####################
### Price Treand   ###
#####################
# version 1
alldata_df<- as.data.frame(alldata)
plot_ETF <- ggplot(alldata_df, aes(x=as.Date(row.names(alldata_df)),y=QQQ))+
  geom_line(aes(color="QQQ"),size=0.8)+
  geom_line(aes(y=alldata_df$XLK,color="XLK"), size=0.7)+
  geom_line(aes(y=alldata_df$VGT,color="VGT"), size=0.7)+
  geom_line(aes(y=alldata_df$FDN,color="FDN"), size=0.7)+
  geom_line(aes(y=alldata_df$IYW,color="IYW"), size=0.7)+
  scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("1 year"))+
  labs(x="Date",y="Price", title="Trends of price",caption = "Source: Yahoo Finanec")+
  scale_color_manual(name="ETF", values=c(QQQ="aquamarine3",XLK="lightpink3",VGT= "cadetblue",
                                          FDN="skyblue1",IYW="orange1"))+
  theme_classic()

plot_index <- ggplot(alldata_df, aes(x=as.Date(row.names(alldata_df)),y=SP500))+
  geom_line(aes(color="SP500"),size=0.7)+
  geom_line(aes(y=alldata_df$DJ,color="DJ"), size=0.7)+
  labs(x="Date",y="Price", title="Trends of price",caption = "Source: Yahoo Finanec and S&P Dow Jones Indices")+
  scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("1 year"))+
  theme_classic()

grid.arrange(plot_ETF, plot_index, nrow=2, ncol=1)

# version 2
par(mfrow=c(3,3))
for (i in colnames(alldata)){
  print(i)
  plot(ts(alldata[,i]),main=i)
}

###################################
### a.1 Descriptive statistics  ###
###################################

returns_no_rf = na.omit(diff(log(alldata[,-8]))*100)
rf = alldata[,8]/52
returns <- na.omit(cbind(returns_no_rf,rf)) 

options(scipen=200) #deciding to print numeric values rather than exponential notation
stats <- function(x){    
  n <- length(x)
  m <- mean(x)
  mid <- median(x)
  s <- sd(x)
  v <- var(x)
  min <- min(x)
  max <- max(x)
  r <- max(x)-min(x)
  fq <- unname(quantile(x, 0.25))
  tq <- unname(quantile(x, 0.75))
  skew <- unname(skewness(x))
  kurt <- unname(kurtosis(x))
  return(c(SampleNo = n, Mean=m, Median=mid, Sd=s,Var=v, Min= min,Max=max,range = r,Qu.1st= fq, Qu.3rd=tq, Skew=skew, Kurt=kurt))
}
results <-  # Descriptive statistics
print(results)data.frame(sapply(returns,stats))



###################################
###  a.2 Plot of distribution   ###
##################################

# QQQ
hst_QQQ <- ggplot(returns, aes(QQQ))+
  geom_histogram(aes(y=..density..),binwidth=0.5,col="#aacfd0", fill="#aacfd0", alpha = .4)+ # change bins and colors
  geom_density(col = '#aacfd0', size = 0.7)+
  stat_function(fun = dnorm, args = list(mean = mean(returns$QQQ), sd = sd(returns$QQQ)),col="#1f4e5f",size = 0.7)+
  labs(x="Log Return",y="Density", caption = "Source: Yahoo Finanec")+ # add the name of x-axis, y-axis, and add reference
  ggtitle("Return's distribution of QQQ")+ # set a title 
  theme_gray() # give graph the theme

# XLK
hst_XLK <- ggplot(returns, aes(XLK))+
  geom_histogram(aes(y=..density..),binwidth=0.5,col="#79a8a9", fill="#79a8a9", alpha = .25)+ # change bins and colors
  geom_density(col = '#79a8a9', size = 0.7)+
  stat_function(fun = dnorm, args = list(mean = mean(returns$XLK), sd = sd(returns$XLK)),col="#1f4e5f")+
  labs(x="Log Return",y="Density", caption = "Source: Yahoo Finanec")+ # add the name of x-axis, y-axis, and add reference
  ggtitle("Return's distribution of XLK")+ # set a title 
  theme_gray() # give graph the theme

# VGT
hst_VGT <- ggplot(returns, aes(VGT))+
  geom_histogram(aes(y=..density..),binwidth=0.5,col="#55967e", fill="#55967e", alpha = .2)+ # change bins and colors
  geom_density(col = '#55967e', size = 0.7)+
  stat_function(fun = dnorm, args = list(mean = mean(returns$VGT), sd = sd(returns$VGT)),col="#1f4e5f")+
  labs(x="Log Return",y="Density", caption = "Source: Yahoo Finanec")+ # add the name of x-axis, y-axis, and add reference
  ggtitle("Return's distribution of VGT")+ # set a title 
  theme_gray() # give graph the theme

# FDN
hst_FDN <- ggplot(returns, aes(FDN))+
  geom_histogram(aes(y=..density..),binwidth=0.5,col="#3b8686", fill="#3b8686", alpha = .25)+ # change bins and colors
  geom_density(col = '#3b8686', size = 0.7)+
  stat_function(fun = dnorm, args = list(mean = mean(returns$FDN), sd = sd(returns$FDN)),col="#1f4e5f")+
  labs(x="Log Return",y="Density", caption = "Source: Yahoo Finanec")+ # add the name of x-axis, y-axis, and add reference
  ggtitle("Return's distribution of FDN")+ # set a title 
  theme_gray() # give graph the theme

# IYW
hst_IYW <- ggplot(returns, aes(IYW))+
  geom_histogram(aes(y=..density..),binwidth=0.5,col="#005f6b", fill="#005f6b", alpha = .3)+ # change bins and colors
  geom_density(col = '#005f6b', size = 0.7)+
  stat_function(fun = dnorm, args = list(mean = mean(returns$IYW), sd = sd(returns$IYW)),col="#1f4e5f")+
  labs(x="Log Return",y="Density", caption = "Source: Yahoo Finanec")+ # add the name of x-axis, y-axis, and add reference
  ggtitle("Return's distribution of IYW")+ # set a title 
  theme_gray() # give graph the theme

# Distrubition graph for FIVE EFT
grid.arrange(hst_QQQ,hst_XLK, hst_VGT,hst_FDN,hst_IYW, nrow=3, ncol=2)

# plot -version 2
chart.Correlation(returns[,-8],method="pearson")

######################################
###  b Covariance & covariance     ###
####################################
cov_mat <- cov(returns[,-8]) # covariance  matrix 
print(cov_mat)
cor_mat = cor(returns[,-8]) #correlation matrix 
print(cor_mat)

# plot - version 1
cor_plot1 <- ggcorrplot(cor_mat,type = "lower",lab = TRUE,hc.order = TRUE,
                    colors = c( "#c6e5d9","white","#79a8a9",al),
                    title="Correlation Matrix",
                    ggtheme = ggplot2::theme_classic)   
print(cor_plot1) # first version

# # plot - verison 2
cor_plot2 <- corrplot(cor_mat) 
print(cor_plot2) # second version


#################################
###   c.1 Tracking error    ####
################################
ETF <- colnames(returns[,seq(1,to=5)])
TE_SP500 <- seq_along(ETF)
TE_DJ <- seq_along(ETF)
for (i in seq_along(ETF)){
  te_SP500 <- TrackingError(returns[,i],returns[,"SP500"])
  TE_SP500 [i] <- te_SP500
  te_DJ <- TrackingError(returns[,i],returns[,"DJ"])
  TE_DJ [i] <- te_DJ
}
TE_SP500 <- t(data.frame(TE_SP500))
TE_DJ <- t(data.frame(TE_DJ))
TE <- rbind(TE_SP500,TE_DJ)
colnames(TE) <- ETF
print(TE)  # results of tracking error of each ETF 


##################################
###   c.3 F-test and t-test   ###
#################################

# F-test for variances
" H0: variances_ETF = variances_index
  Ha: variances_ETF != variances_index"

F_rname <- c("F_test_SP500","F_test_DJ")
F_test <-matrix(0, nrow=2,ncol=5, byrow=TRUE, dimnames = list(F_rname,ETF)) 
for (i in seq_along(ETF)){
  F_test_SP500 <-  var.test(returns[,ETF[i]],returns[,"SP500"])
  F_test_DJ <-  var.test(returns[,ETF[i]],returns[,"DJ"])
  F_test[,i] <- c(F_test_SP500$p.value,F_test_DJ$p.value)
}

# t-test for means
" Ho:  mean_ETF  =  mean_index
  Ha:  mean_ETF !=  mean_index"
p_cri= 0.05
T_rname <- c("T_test_SP500","T_test_DJ")
T_test <- matrix(0, nrow=2,ncol=5, byrow=TRUE, dimnames = list(T_rname,ETF)) 
for (i in seq_along(ETF)){ 
  if ((F_test["F_test_SP500",ETF[i]]<p_cri) ==TRUE){
    t1 <- t.test(returns[,ETF[i]],returns[,"SP500"], paired = FALSE, var.equal = FALSE)
    T_test["T_test_SP500",ETF[i]] <- t1$p.value
  }else {
    t2 <- t.test(returns[,ETF[i]],returns[,"SP500"], paired = FALSE, var.equal = TRUE)
    T_test["T_test_SP500",ETF[i]] <- t2$p.value
  }}

for (i in seq_along(ETF)){   
  if ((F_test["F_test_DJ",ETF[i]]< p_cri) ==TRUE){
    t3 <- t.test(returns[,ETF[i]],returns[,"DJ"], paired = FALSE, var.equal = FALSE)
    T_test["T_test_DJ",ETF[i]] <- t3$p.value
  }else{
    t4 <- t.test(returns[,ETF[i]],returns[,"DJ"], paired = FALSE, var.equal = TRUE)
    T_test["T_test_DJ",ETF[i]] <- t4$p.value
  }}
Test_p_value <- list("F-test for variances:"=F_test,
                     "T-test for means:"=T_test) 

Test_summary <- list("F-test for variances:"=F_test< p_cri,
                     "T-test for means:"=T_test< p_cri,
                     "Explain" = c("F-test - TRUE:  Reject H0         |  Variance of ETF is unequl to variance of index",
                                   "F-test - FALSE: Cannot Reject H0  |  Variance of ETF is equl to variance of index",
                                   "T-test - TRUE:  Reject H0,         |  Mean of ETF is unequal to mean of index",
                                   "T-test - FALSE: Cannot Reject H0  |  Mean of ETF is equal to mean of index"))
print(Test_summary) # summary of F-test and t-test 



######################################
###  d  Excess return of the data   ###
######################################

class(rname)
as.character()
r <- dim(returns)[1]
c <- dim(returns)[2]-1
rname <- as.character(index(returns))
cname <- colnames(returns[,-8])
returns_ex <- matrix(0,nrow=r,ncol=c, 
                     dimnames = list(rname,cname))
for (i in 1:c){
  returns_ex[,i] <-  returns[,i]-returns$rf
}
returns_ex <- data.frame(returns_ex)
describe(returns_ex)
str(returns_ex)


##############################
###  d Linear regression   ###
#############################

#1) QQQ
lm_QQQ_SP500 = lm(QQQ~SP500, data=returns_ex)
summary(lm_QQQ_SP500)
anova(lm_QQQ_SP500)

lm_QQQ_DJ = lm(QQQ~DJ, data=returns_ex)
summary(lm_QQQ_DJ)
anova(lm_QQQ_DJ)

# 2) XLK
lm_XLK_SP500 = lm(XLK~SP500, data=returns_ex)
summary(lm_XLK_SP500)
anova(lm_XLK_SP500)

lm_XLK_DJ = lm(XLK~DJ, data=returns_ex)
summary(lm_XLK_DJ)
anova(lm_XLK_DJ)

# 3) VGT
lm_VGT_SP500 = lm(VGT~SP500, data=returns_ex)
summary(lm_VGT_SP500)
anova(lm_VGT_SP500)

lm_VGT_DJ = lm(VGT~DJ, data=returns_ex)
summary(lm_VGT_DJ)
anova(lm_VGT_DJ)

# 4) FDN
lm_FDN_SP500 = lm(FDN~SP500, data=returns_ex)
summary(lm_FDN_SP500)
anova(lm_FDN_SP500)

lm_FDN_DJ = lm(FDN~DJ, data=returns_ex)
summary(lm_FDN_DJ)
anova(lm_FDN_DJ)

# 5) IYW
lm_IYW_SP500 = lm(IYW~SP500, data=returns_ex)
summary(lm_IYW_SP500)
anova(lm_IYW_SP500)

lm_IYW_DJ = lm(IYW~DJ, data=returns_ex)
summary(lm_IYW_DJ)
anova(lm_IYW_DJ)

# regresion plot
# QQQ
lr_QQQ_SP500 <- ggplot(returns_ex, aes(SP500, QQQ)) +
  geom_point(size=3.5,color= "#aacfd0",shape=4)+
  labs(x="SP500",y="QQQ",title ="Linear relationship-QQQ&SP500")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

lr_QQQ_DJ <- ggplot(returns_ex, aes(DJ, QQQ)) +
  geom_point(size=3.5,color= "#aacfd0",shape=6)+
  labs(x="DJ",y="QQQ",title ="Linear relationship-QQQ&DJ")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

# XLK
lr_XLK_SP500 <- ggplot(returns_ex, aes(SP500, XLK)) +
  geom_point(size=3.5,color= "#79a8a9",shape=4)+
  labs(x="SP500",y="XLK",title ="Linear relationship-XLK&SP500")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

lr_XLK_DJ <- ggplot(returns_ex, aes(DJ, XLK)) +
  geom_point(size=3.5,color= "#79a8a9",shape=6)+
  labs(x="DJ",y="XLK",title ="Linear relationship-XLK&DJ")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

# VGT
lr_VGT_SP500 <- ggplot(returns_ex, aes(SP500, VGT)) +
  geom_point(size=3.5,color= "#55967e",shape=4)+
  labs(x="SP500",y="VGT",title ="Linear relationship-VGT&SP500")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

lr_VGT_DJ <- ggplot(returns_ex, aes(DJ, VGT)) +
  geom_point(size=3.5,color= "#55967e",shape=6)+
  labs(x="DJ",y="VGT",title ="Linear relationship-VGT&DJ")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

# FDN
lr_FDN_SP500 <- ggplot(returns_ex, aes(SP500, FDN)) +
  geom_point(size=3.5,color= "#3b8686",shape=4)+
  labs(x="SP500",y="FDN",title ="Linear relationship-FDN&SP500")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

lr_FDN_DJ <- ggplot(returns_ex, aes(DJ, FDN)) +
  geom_point(size=3.5,color= "#3b8686",shape=6)+
  labs(x="DJ",y="FDN",title ="Linear relationship-FDN&DJ")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

# IYW
lr_IYW_SP500 <- ggplot(returns_ex, aes(SP500, IYW)) +
  geom_point(size=3.5,color= "#005f6b",shape=4)+
  labs(x="SP500",y="IYW",title ="Linear relationship-FDN&SP500")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

lr_IYW_DJ <- ggplot(returns_ex, aes(DJ, IYW)) +
  geom_point(size=3.5,color= "#005f6b",shape=6)+
  labs(x="DJ",y="IYW",title ="Linear relationship-IYW&DJ")+
  geom_smooth(method = "lm", color = "#1f4e5f",size=0.7,se = FALSE)+
  theme_classic(base_size=9)

grid.arrange(lr_QQQ_SP500,lr_QQQ_DJ ,lr_XLK_SP500,lr_XLK_DJ,lr_VGT_SP500,lr_VGT_DJ,
             lr_FDN_SP500,lr_FDN_DJ,lr_IYW_SP500,lr_IYW_DJ, 
             nrow=5, ncol=2)

#########################################
###  e.1 Fama-French 3 factors - DATA ###
#########################################

# pull the factor data from Dr French's website
temp = tempfile()
base = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
factor = "F-F_Research_Data_Factors_Daily"
format =  "_TXT.zip"
full_url =  glue(base, factor,  format,  sep ="")

#Now we pass full_url to download.file().

download.file(full_url,temp,quiet = TRUE)

#Finally, we can read the txt file using read_table() after unzipping that data with the unz() function.

ff_3factors =  read.table(unz(temp,"F-F_Research_Data_Factors_daily.txt"),
                          skip = 4, header=TRUE)

ff_3factors <- na.omit(ff_3factors[-nrow(ff_3factors),]) # remove the last line which is copyright
Date <-  anydate(rownames(ff_3factors))
ff_3factors <- xts(ff_3factors ,Date,format='%Y-%m-%d')
dates <- seq(as.Date("2014-01-01"), to=as.Date("2019-10-01"), by="weeks") 
ff_3factors <- as.data.frame(ff_3factors[c(dates),])
ff_all <- cbind(returns_ex[,seq(1,5)],ff_3factors[-1,-4])
ff_all <- ff_all %>%
  mutate(SMB=as.numeric(as.character(SMB)),
         HML=as.numeric(as.character(HML)),
         Mkt.RF=as.numeric(as.character(Mkt.RF)))


#####################################################
###  e.2 Fama-French 3 factors - Linear regresion ###
####################################################

lm_QQQ_ff <- lm(QQQ~Mkt.RF+SMB+HML,data=ff_all)
summary(lm_QQQ_ff)


lm_XLK_ff <- lm(XLK~Mkt.RF+SMB+HML,data=ff_all)
summary(lm_XLK_ff)

lm_VGT_ff <- lm(VGT~Mkt.RF+SMB+HML,data=ff_all)
summary(lm_VGT_ff)

lm_FDN_ff <- lm(FDN~Mkt.RF+SMB+HML,data=ff_all)
summary(lm_FDN_ff)

lm_IYW_ff <- lm(IYW~Mkt.RF+SMB+HML,data=ff_all)
summary(lm_IYW_ff)


###############################
###    f.1 assumation      ###
##############################

# 1  Linearity
par(mfrow=c(3,2))
crPlots(lm_QQQ_SP500)
crPlots(lm_XLK_SP500)
crPlots(lm_VGT_SP500)
crPlots(lm_FDN_SP500)
crPlots(lm_IYW_SP500)

par(mfrow=c(3,2))
crPlots(lm_QQQ_DJ)
crPlots(lm_XLK_DJ)
crPlots(lm_VGT_DJ)
crPlots(lm_FDN_DJ)
crPlots(lm_IYW_DJ)

crPlots(lm_QQQ_ff)
crPlots(lm_XLK_ff)
crPlots(lm_VGT_ff)
crPlots(lm_FDN_ff)
crPlots(lm_IYW_ff)

# 2 Normality
par(mfrow=c(3,2))
qqPlot(lm_QQQ_SP500)
qqPlot(lm_XLK_SP500)
qqPlot(lm_VGT_SP500)
qqPlot(lm_FDN_SP500)
qqPlot(lm_IYW_SP500)

par(mfrow=c(3,2))
qqPlot(lm_QQQ_DJ)
qqPlot(lm_XLK_DJ)
qqPlot(lm_VGT_DJ)
qqPlot(lm_FDN_DJ)
qqPlot(lm_IYW_DJ)

par(mfrow=c(3,2))
qqPlot(lm_QQQ_ff)
qqPlot(lm_XLK_ff)
qqPlot(lm_VGT_ff)
qqPlot(lm_FDN_ff)
qqPlot(lm_IYW_ff)

# White test - Heteroskedasticity
bptest(lm_QQQ_SP500, ~ I(SP500^2), data = returns_ex) # Homoscedasticity
bptest(lm_XLK_SP500, ~ I(SP500^2), data = returns_ex) # Homoscedasticity
bptest(lm_VGT_SP500, ~ I(SP500^2), data = returns_ex) # Homoscedasticity
bptest(lm_FDN_SP500, ~ I(SP500^2), data = returns_ex) # Homoscedasticity
bptest(lm_IYW_SP500, ~ I(SP500^2), data = returns_ex) # Homoscedasticity

bptest(lm_QQQ_SP500, ~ I(DJ^2), data = returns_ex) # Homoscedasticity
bptest(lm_XLK_SP500, ~ I(DJ^2), data = returns_ex) # Homoscedasticity
bptest(lm_VGT_SP500, ~ I(DJ^2), data = returns_ex) # Homoscedasticity
bptest(lm_FDN_SP500, ~ I(DJ^2), data = returns_ex) # Homoscedasticity
bptest(lm_IYW_SP500, ~ I(DJ^2), data = returns_ex) # Homoscedasticity

bptest(lm_QQQ_ff, ~ I(Mkt.RF^2)+I(SMB^2)+I(HML^2)+
         Mkt.RF*SMB+Mkt.RF*HML+SMB*HML, data = ff_all) # Homoscedasticity

bptest(lm_XLK_ff, ~ I(Mkt.RF^2)+I(SMB^2)+I(HML^2)+
         Mkt.RF*SMB+Mkt.RF*HML+SMB*HML, data = ff_all) # Homoscedasticity

bptest(lm_VGT_ff, ~ I(Mkt.RF^2)+I(SMB^2)+I(HML^2)+
         Mkt.RF*SMB+Mkt.RF*HML+SMB*HML, data = ff_all) # Homoscedasticity

bptest(lm_FDN_ff, ~ I(Mkt.RF^2)+I(SMB^2)+I(HML^2)+
         Mkt.RF*SMB+Mkt.RF*HML+SMB*HML, data = ff_all) # Homoscedasticity

bptest(lm_IYW_ff, ~ I(Mkt.RF^2)+I(SMB^2)+I(HML^2)+
         Mkt.RF*SMB+Mkt.RF*HML+SMB*HML, data = ff_all) # Homoscedasticity

# Breusch¨CGodfrey test - Autocorrelation
bgtest(lm_QQQ_SP500) # Non-autocorrelation
bgtest(lm_XLK_SP500) # Non-autocorrelation
bgtest(lm_VGT_SP500) # Non-autocorrelation
bgtest(lm_FDN_SP500) # Non-autocorrelation
bgtest(lm_IYW_SP500) # Non-autocorrelation

bgtest(lm_QQQ_DJ) # Autocorrelation
bgtest(lm_XLK_DJ) # Autocorrelation
bgtest(lm_VGT_DJ) # Autocorrelation
bgtest(lm_FDN_DJ) # Autocorrelation
bgtest(lm_IYW_DJ) # Autocorrnelatio

bgtest(lm_QQQ_ff) # Non-autocorrelation
bgtest(lm_XLK_ff) # Non-autocorrelation
bgtest(lm_VGT_ff) # Non-autocorrelation
bgtest(lm_FDN_ff) # Non-autocorrelation
bgtest(lm_IYW_ff) # Non-autocorrelation

# VIF - Multicollinearity
vif(lm_QQQ_ff) # non-multicollinearity


###################@@#####
###     f.2 Remedy     ###
##########################
coeftest(lm_QQQ_DJ, vcov = NeweyWest(lm_QQQ_DJ))
coeftest(lm_XLK_DJ, vcov = NeweyWest(lm_XLK_DJ))
coeftest(lm_VGT_DJ, vcov = NeweyWest(lm_VGT_DJ))
coeftest(lm_FDN_DJ, vcov = NeweyWest(lm_FDN_DJ))
coeftest(lm_IYW_DJ, vcov = NeweyWest(lm_IYW_DJ))




