rm(list=ls())




### packages ###########################################################################
require(readr)
require(dplyr)
require(matrixStats)




### data ##########################################################################
OM <- read_delim('C:/Users/ricar/OneDrive - ufmg.br/Mort Materna/ipc21/R/OM2009_final.csv')
NV <- read_delim('C:/Users/ricar/OneDrive - ufmg.br/Mort Materna/ipc21/R/NV2009_final.csv')
dim(OM)
dim(NV)
cbind(names(OM),names(NV))


ages = c(10,15,20,25,30,35,40,45,50)
years = NV %>% pull(Ano) %>% unique()


OMy <- OM %>% group_by(Ano) %>% summarise_at(vars(ends_with("anos")), sum) %>%
  data.table::setnames(new=c("ano",ages))

NVy <- NV %>% group_by(Ano) %>% summarise_at(vars(ends_with("anos")), sum) %>%
  data.table::setnames(new=c("ano",ages))

mxt <- bind_cols(NVy %>% select(ano),
                 OMy %>% select(!contains("ano")) / NVy %>% select(!contains("ano")))

# summary(log(mxt))
# mxt <- mxt %>% select(-'50')
# ages = c(10,15,20,25,30,35,40,45)


{
par(mfrow=c(1,1))
# postscript(paste0(tex_path,'log_mmr.eps'), fonts=c("serif"),
#           width=9, height=4, horizontal=F, onefile=F, paper="special", colormodel="cmyk")
plot(y=log(mxt %>% filter(ano==2009) %>% select(-ano)), x=ages, ylim=c(-8,-3),
     xlab="Age groups", ylab="Observed log(mmr)", type="n", lty="blank")
lines(y=log(mxt %>% filter(ano==2009) %>% select(-ano)), x=ages, lty="dotted", lwd=2, col="red", type='b', pch=1)
lines(y=log(mxt %>% filter(ano==2014) %>% select(-ano)), x=ages, lty="longdash", lwd=2, col="black", type='b', pch=4)
lines(y=log(mxt %>% filter(ano==2019) %>% select(-ano)), x=ages, lty="solid", lwd=2, col="blue", type='b', pch=20)
legend(10,-5, bty='n',
       c("2009","2014","2019"),
       lty=c("dotted","longdash","solid"),
       lwd=c(1.5),
       pch = c(1,4,20),
       col=c("red","black","blue"),
       text.col=c("red","black","blue"))
# dev.off()
}


{
par(mfrow=c(1,1))
# postscript(paste0(tex_path,'mmr.eps'), fonts=c("serif"),
#           width=9, height=4, horizontal=F, onefile=F, paper="special", colormodel="cmyk")
plot(y = mxt %>% filter(ano==2009) %>% select(-ano), x=ages, ylim=c(0,0.006),
     xlab="Age groups", ylab="Observed mmr", type="n", lty="blank")
lines(y=mxt %>% filter(ano==2009) %>% select(-ano), x=ages, lty="dotted", lwd=2, col="red", type='b', pch=1)
lines(y=mxt %>% filter(ano==2014) %>% select(-ano), x=ages, lty="longdash", lwd=2, col="black", type='b', pch=4)
lines(y=mxt %>% filter(ano==2019) %>% select(-ano), x=ages, lty="solid", lwd=2, col="blue", type='b', pch=20)
legend(10,0.006, bty='n',
       c("2009","2014","2019"),
       lty=c("dotted","longdash","solid"),
       lwd=c(1.5),
       pch = c(1,4,20),
       col=c("red","black","blue"),
       text.col=c("red","black","blue"))
# dev.off()
}



Y = t(as.matrix(log(mxt[,-c(1,10)])))
colnames(Y) = years

# age 45+
OMy[,9] = OMy[,9] + OMy[,10]
NVy[,9] = NVy[,9] + NVy[,10]

Dxt = t(OMy[,-c(1,10)])
Nxt = t(NVy[,-c(1,10)])
r = nrow(Y); r
n = ncol(Y); n



### LC (mle) ########################################################################

# initial values
# a = log(rowSums(Dxt)/rowSums(Nxt))
a = log(rowSums(Dxt/Nxt))
b = rep(1,r)/r # sum(b)=1
# k = rep(0,n) # sum(k)=0
k = rnorm(n,0,1)
k = k-mean(k) # sum(k)=0

M = matrix(nrow=r, ncol=n)
for (t in 1:n) {M[,t] = a + b*k[t]}
L = sum(Dxt*M - Nxt*exp(M))

d = 1e-100
dL = 1
it = 0


while (abs(dL) >= d)  {

  # update a
  for (x in 1:r) {
    a[x] = a[x] + sum( Dxt[x,]-Nxt[x,]*exp(a[x]+b[x]*k) ) / sum( Nxt[x,]*exp(a[x]+b[x]*k) ) # newton
  }

  # update k
  for (t in 1:n) {
    k[t] = k[t] + sum( (Dxt[,t]-Nxt[,t]*exp(a+b*k[t]))*b ) / sum( Nxt[,t]*exp(a+b*k[t])*b^2 ) # newton
  }
  # # adjust k to sum 0
  # k = k-mean(k)
  # k = (k-mean(k))*sum(b)

  # update b
  for (x in 1:r) {
    b[x] = b[x] + sum( (Dxt[x,]-Nxt[x,]*exp(a[x]+b[x]*k))*k ) / sum( Nxt[x,]*exp(a[x]+b[x]*k)*k^2 ) # newton
  }
  # b = b/sum(b)

  
  # b,k adjust
  # k = (k-mean(k))*sum(b)
  # b = b/sum(b)

  # calculate dS
  for (t in 1:n) {M[,t] = a + b*k[t]}
  dL = L/sum(Dxt*M - Nxt*exp(M)) - 1
  L = sum(Dxt*M - Nxt*exp(M))

  # iterations
  it = it + 1
  
}

it
L


# a,b,k adjust
# Delwarde etal (2006) Application of the Poisson log-bilinear projection model to G5
a = a + b*mean(k)
k = (k-mean(k))*sum(b)
b = b/sum(b)








### k forecast
k.fit = forecast::Arima(k, order = c(0,1,0), include.drift=T)

h = 11
years.fut = (years[n]+1):(years[n]+h)
alpha = 0.05
q = qnorm(1-alpha/2)
kfut = array(NA_real_,c(3,h), dimnames=list(c('pred','ic0','ic1')))
yfut = array(NA_real_,c(3,r,h), dimnames=list(c('pred','ic0','ic1'),NULL,years.fut))

for (t in 1:h){
  sd.k = sqrt((t^2*k.fit$var.coef[1] + t*k.fit$sigma2))

  # kfut
  # kfut['pred',t] = k.fit$fitted[n] + t*k.fit$coef[1]
  kfut['pred',t] = k[n] + t*k.fit$coef[1]
  kfut['ic0',t] = kfut['pred',t] - q*sd.k
  kfut['ic1',t] = kfut['pred',t] + q*sd.k

  # yfut
  yfut['pred',,t] = a + b*kfut['pred',t]
  yfut['ic0',,t] = a + b*kfut['ic0',t]
  yfut['ic1',,t] = a + b*kfut['ic1',t]
}


### k plot
par(mfrow=c(1,1))
plot(y=c(k,rep(NA,h)), x=c(years,years.fut), type="l", col="black", ylim=c(-8,4), xlab='', ylab='')
lines(y=kfut['pred',], x=years.fut, type="l", col="darkblue", lty="longdash")
lines(y=kfut['ic0',], x=years.fut, type="l", col="darkblue", lty="solid")
lines(y=kfut['ic1',], x=years.fut, type="l", col="darkblue", lty="solid")


### yfut plot (log)
par(mfrow=c(1,1))
plot(y=yfut['pred',,'2030'], x=ages[1:r], ylim=c(-9,-4), xlab="Age groups", ylab="log(mmr)", type="n", lty="blank", cex.axis=0.8, xaxt='n')
segments(x0=ages[1:r], y0=yfut['ic0',,'2030'], y1=yfut['ic1',,'2030'], col='blue', lty='solid', lwd=0.5)
points(y=yfut['pred',,'2030'], x=ages[1:r], pch=20, lwd=1.5, col='darkblue', cex=1.1)
points(y=Y[,'2019'], x=ages[1:r], lwd=2, col="darkred", pch=4, cex=.9)
legend(10,-4, bty='n', cex=0.8,
       c("2019 observed","2030 predicted","2030 intervals (95%)"),
       lty=c(NA,NA,1),
       pch=c(4,20,NA),
       lwd=c(2,2,1.5),
       col=c("darkred","darkblue","blue"),
       text.col=c("darkred","darkblue","blue"))
axis(1,at=ages[1:r],labels=ages[1:r], cex.axis=0.9)



### yfut plot
par(mfrow=c(1,1))
plot(y=exp(yfut['pred',,'2030']), x=ages[1:r], ylim=c(0,0.0045), xlab="Age groups", ylab="mmr", type="n", lty="blank", cex.axis=0.8, xaxt='n')
segments(x0=ages[1:r], y0=exp(yfut['ic0',,'2030']), y1=exp(yfut['ic1',,'2030']), col='blue', lty='solid', lwd=0.5)
points(y=exp(yfut['pred',,'2030']), x=ages[1:r], pch=20, lwd=1.5, col='darkblue', cex=0.8)
points(y=exp(Y[,'2019']), x=ages[1:r], lwd=2, col="darkred", pch=4, cex=.9)
legend(10,0.0045, bty='n', cex=0.8,
       c("2019 observed","2030 predicted","2030 intervals (95%)"),
       lty=c(NA,NA,1),
       pch=c(4,20,NA),
       lwd=c(2,2,1.5),
       col=c("darkred","darkblue","blue"),
       text.col=c("darkred","darkblue","blue"))
axis(1,at=ages[1:r],labels=ages[1:r], cex.axis=0.9)




{
# postscript(paste0(tex_path,'pred_ages.eps'), fonts=c("serif"),
#            width=11, height=6, horizontal=F, onefile=F, paper="special", colormodel="cmyk")
 
par(mfrow=c(2,4))

plot(y=c(Y[1,],yfut[1,1,]), x=c(years,(max(years)+1):(max(years)+h)), ylim=c(-9,-4),
     xlab='years', ylab='log(mmr)', type='b', pch=20, cex.axis=1.1, cex.lab=1.2,
     main='ages 10-14')
lines(y=yfut[2,1,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)
lines(y=yfut[3,1,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)

plot(y=c(Y[2,],yfut[1,2,]), x=c(years,(max(years)+1):(max(years)+h)), ylim=c(-9,-4),
     xlab='years', ylab='', type='b', pch=20, cex.axis=1.1, cex.lab=1.2,
     main='ages 15-19')
lines(y=yfut[2,2,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)
lines(y=yfut[3,2,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)

plot(y=c(Y[3,],yfut[1,3,]), x=c(years,(max(years)+1):(max(years)+h)), ylim=c(-9,-4),
     xlab='years', ylab='', type='b', pch=20, cex.axis=1.1, cex.lab=1.2,
     main='ages 20-24')
lines(y=yfut[2,3,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)
lines(y=yfut[3,3,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)

plot(y=c(Y[4,],yfut[1,4,]), x=c(years,(max(years)+1):(max(years)+h)), ylim=c(-9,-4),
     xlab='years', ylab='', type='b', pch=20, cex.axis=1.1, cex.lab=1.2,
     main='ages 25-29')
lines(y=yfut[2,4,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)
lines(y=yfut[3,4,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)

plot(y=c(Y[5,],yfut[1,5,]), x=c(years,(max(years)+1):(max(years)+h)), ylim=c(-9,-4),
     xlab='years', ylab='log(mmr)', type='b', pch=20, cex.axis=1.1, cex.lab=1.2,
     main='ages 30-34')
lines(y=yfut[2,5,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)
lines(y=yfut[3,5,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)

plot(y=c(Y[6,],yfut[1,6,]), x=c(years,(max(years)+1):(max(years)+h)), ylim=c(-9,-4),
     xlab='years', ylab='', type='b', pch=20, cex.axis=1.1, cex.lab=1.2,
     main='ages 35-39')
lines(y=yfut[2,6,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)
lines(y=yfut[3,6,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)

plot(y=c(Y[7,],yfut[1,7,]), x=c(years,(max(years)+1):(max(years)+h)), ylim=c(-9,-4),
     xlab='years', ylab='', type='b', pch=20, cex.axis=1.1, cex.lab=1.2,
     main='ages 40-44')
lines(y=yfut[2,7,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)
lines(y=yfut[3,7,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)

plot(y=c(Y[8,],yfut[1,8,]), x=c(years,(max(years)+1):(max(years)+h)), ylim=c(-9,-4),
     xlab='years', ylab='', type='b', pch=20, cex.axis=1.1, cex.lab=1.2,
     main='ages 45-49')
lines(y=yfut[2,8,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)
lines(y=yfut[3,8,], x=(max(years)+1):(max(years)+h), lty='solid', lwd=0.5)

# dev.off()
}


